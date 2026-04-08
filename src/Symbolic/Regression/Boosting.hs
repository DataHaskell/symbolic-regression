{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | E-Graph Boosted Symbolic Regression.

Stagewise additive construction of symbolic expressions with unconstrained
grammar and periodic e-graph rewriting.

Each round generates small random expressions (max 5 nodes) from the full
operator set, fits their parameters against residuals via NLOPT, and adds
the best one — but only if it improves validation MSE.

Every K rounds the entire ensemble is collapsed into a single simplified
expression via full e-graph saturation (including distributivity).  This
keeps expressions compact throughout training rather than accumulating
hundreds of additive terms.

Stops immediately when training MSE reaches zero.
-}
module Symbolic.Regression.Boosting (
    -- * Main API
    fitBoosted,

    -- * Configuration
    BoostConfig (..),
    defaultBoostConfig,

    -- * Results
    BoostResult (..),

    -- * Internals (exported for testing)
    ActiveTerm (..),
    BoostState (..),
    randomExpr,
    buildEnsembleExpr,
    collapseEnsemble,
    pruneByContribution,
    generateAndScore,
    partialEval,
) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.List (maximumBy, partition, sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
import System.Random (StdGen, mkStdGen, randomR)

import Symbolic.Regression.EGraph (
    ClassId,
    SREGraph,
    emptySREGraph,
    getBestExpr,
    insertTree,
    runSaturationNWith,
 )
import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree)
import Symbolic.Regression.Expr.Opt (minimizeNLLRidge)
import Symbolic.Regression.Expr.Utils (
    constsToParam,
    countNodes,
    paramsToConst,
    relabelParams,
 )
import Symbolic.Regression.Fitness (DataSet, evaluateFitness, mseScore)
import Symbolic.Regression.Poly (polySimplify)
import qualified Symbolic.Regression.Rewrites as RW

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

data BoostConfig = BoostConfig
    { bcRounds :: !Int
    -- ^ Maximum boosting rounds (default: 200)
    , bcLearningRate :: !Double
    -- ^ Shrinkage η (default: 0.1)
    , bcRefitEvery :: !Int
    -- ^ M: joint refit period in rounds (default: 10)
    , bcRefitRidge :: !Double
    -- ^ ρ: ridge penalty for joint refit (default: 0.01)
    , bcSimplifyEvery :: !Int
    -- ^ K: full e-graph rewrite + collapse period (default: 10)
    , bcNloptIters :: !Int
    -- ^ NLOPT iterations per candidate (default: 30)
    , bcNloptRetries :: !Int
    -- ^ Parameter retries per candidate (default: 2)
    , bcPruneThreshold :: !Double
    -- ^ Contribution-norm pruning threshold (default: 0.02)
    , bcEarlyStopPat :: !Int
    -- ^ Early stopping patience in rounds (default: 20)
    , bcMaxWeakLearnerSize :: !Int
    -- ^ Max AST nodes per random candidate (default: 7)
    , bcCandidatesPerRound :: !Int
    -- ^ Random candidates to generate per round (default: 50)
    , bcSatIters :: !Int
    -- ^ Saturation iterations for periodic collapse (default: 30)
    , bcShowTrace :: !Bool
    -- ^ Print progress to stderr (default: True)
    }
    deriving (Show)

defaultBoostConfig :: BoostConfig
defaultBoostConfig =
    BoostConfig
        { bcRounds = 200
        , bcLearningRate = 0.1
        , bcRefitEvery = 10
        , bcRefitRidge = 0.01
        , bcSimplifyEvery = 5
        , bcNloptIters = 30
        , bcNloptRetries = 2
        , bcPruneThreshold = 0.02
        , bcEarlyStopPat = 20
        , bcMaxWeakLearnerSize = 7
        , bcCandidatesPerRound = 50
        , bcSatIters = 30
        , bcShowTrace = True
        }

------------------------------------------------------------------------
-- Core data types
------------------------------------------------------------------------

-- | An active term in the ensemble.
data ActiveTerm = ActiveTerm
    { atExpr :: !(Fix ExprF)
    -- ^ Expression with constants substituted (no ParamF)
    , atCoeff :: !Double
    -- ^ Effective coefficient (updated by refit)
    , atTrainVec :: !PVector
    -- ^ Cached evaluation on training rows
    , atValVec :: !PVector
    -- ^ Cached evaluation on validation rows
    , atClassId :: !(Maybe ClassId)
    -- ^ E-class in persistent graph
    }

-- | Scored candidate from random generation.
data CandidateScore = CandidateScore
    { csExpr :: !(Fix ExprF)
    , csTrainVec :: !PVector
    , csValVec :: !PVector
    , csCoeff :: !Double
    , csGain :: !Double
    }

-- | Mutable state for the boosting loop.
data BoostState = BoostState
    { bsRound :: !Int
    , bsBias :: !Double
    , bsTerms :: ![ActiveTerm]
    , bsTrainPreds :: !PVector
    , bsValPreds :: !PVector
    , bsResiduals :: !PVector
    , bsGraph :: !SREGraph
    , bsValHistory :: ![Double]
    , bsElite :: ![Fix ExprF]
    -- ^ Top-K candidate expressions from previous rounds (for mutation)
    }

-- | Result of a boosting run.
data BoostResult = BoostResult
    { brEnsembleExpr :: Fix ExprF
    , brDistilled :: [Fix ExprF]
    , brTerms :: [ActiveTerm]
    , brTrainMSE :: Double
    , brValMSE :: Double
    }

------------------------------------------------------------------------
-- Random expression generation
------------------------------------------------------------------------

-- | Available unary operators for random generation.
unaryOps :: [UnOp]
unaryOps = [Neg, Abs, Recip, Sq, Sqrt, Exp, Log, Sin, Cos]

-- | Available binary operators for random generation.
binaryOps :: [BinOp]
binaryOps = [Add, Sub, Mul, Div]

-- | Generate a random expression tree with at most @maxSize@ nodes.
randomExpr :: Int -> Int -> StdGen -> (Fix ExprF, StdGen)
randomExpr maxSize nFeatures g0
    | maxSize <= 1 = randomTerminal nFeatures g0
    | otherwise =
        let (coin, g1) = randomR (0 :: Int, 99) g0
         in if coin < 30
                then randomTerminal nFeatures g1
                else
                    if coin < 60
                        then -- unary op
                            let (opIdx, g2) = randomR (0, length unaryOps - 1) g1
                                op = unaryOps !! opIdx
                                (child, g3) = randomExpr (maxSize - 1) nFeatures g2
                             in (Fix (UnF op child), g3)
                        else -- binary op
                            let (opIdx, g2) = randomR (0, length binaryOps - 1) g1
                                op = binaryOps !! opIdx
                                budget = maxSize - 1
                                (splitPt, g3) = randomR (1, max 1 (budget - 1)) g2
                                (left, g4) = randomExpr splitPt nFeatures g3
                                (right, g5) = randomExpr (budget - splitPt) nFeatures g4
                             in (Fix (BinF op left right), g5)

-- | Generate a random terminal: variable or learnable parameter.
randomTerminal :: Int -> StdGen -> (Fix ExprF, StdGen)
randomTerminal nFeatures g0 =
    let (coin, g1) = randomR (0 :: Int, 99) g0
     in if coin < 60
            then -- variable
                let (j, g2) = randomR (0, max 0 (nFeatures - 1)) g1
                 in (var j, g2)
            else -- parameter
                let (i, g2) = randomR (0, 3) g1
                 in (param i, g2)

------------------------------------------------------------------------
-- Expression mutation
------------------------------------------------------------------------

-- | Mutate an expression tree. Picks a random mutation type.
mutateExpr :: Int -> Int -> Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutateExpr maxSize nFeatures expr g0 =
    let (coin, g1) = randomR (0 :: Int, 99) g0
     in if coin < 25
            then mutateReplaceSubtree maxSize nFeatures expr g1
            else
                if coin < 50
                    then mutateChangeOp expr g1
                    else
                        if coin < 75
                            then mutateInsertNode maxSize nFeatures expr g1
                            else mutateDeleteNode expr g1

-- | Replace a random subtree with a new random tree.
mutateReplaceSubtree :: Int -> Int -> Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutateReplaceSubtree maxSize nFeatures expr g0 =
    let sz = countNodes expr
        (targetIdx, g1) = randomR (0, max 0 (sz - 1)) g0
     in replaceAt targetIdx maxSize nFeatures expr g1
  where
    replaceAt 0 ms nf _ g = randomExpr (min 5 ms) nf g
    replaceAt n ms nf (Fix (UnF op c)) g =
        let (c', g') = replaceAt (n - 1) ms nf c g in (Fix (UnF op c'), g')
    replaceAt n ms nf (Fix (BinF op l r)) g =
        let lsz = countNodes l
         in if n - 1 < lsz
                then let (l', g') = replaceAt (n - 1) ms nf l g in (Fix (BinF op l' r), g')
                else
                    let (r', g') = replaceAt (n - 1 - lsz) ms nf r g in (Fix (BinF op l r'), g')
    replaceAt n ms nf (Fix (SumF xs)) g = replaceInList n ms nf SumF xs g
    replaceAt n ms nf (Fix (ProdF xs)) g = replaceInList n ms nf ProdF xs g
    replaceAt _ ms nf _ g = randomExpr (min 5 ms) nf g

    replaceInList _ ms nf wrap [] g = let (e, g') = randomExpr (min 5 ms) nf g in (Fix (wrap [e]), g')
    replaceInList n ms nf wrap (x : xs) g =
        let xsz = countNodes x
         in if n - 1 < xsz
                then let (x', g') = replaceAt (n - 1) ms nf x g in (Fix (wrap (x' : xs)), g')
                else
                    let (restResult, g') = replaceInList (n - 1 - xsz) ms nf wrap xs g
                     in case restResult of
                            Fix (SumF rest) -> (Fix (SumF (x : rest)), g')
                            Fix (ProdF rest) -> (Fix (ProdF (x : rest)), g')
                            other -> (Fix (wrap [x, other]), g')

-- | Change the operator of a random node.
mutateChangeOp :: Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutateChangeOp (Fix (UnF _ c)) g =
    let (idx, g') = randomR (0, length unaryOps - 1) g
     in (Fix (UnF (unaryOps !! idx) c), g')
mutateChangeOp (Fix (BinF _ l r)) g =
    let (idx, g') = randomR (0, length binaryOps - 1) g
     in (Fix (BinF (binaryOps !! idx) l r), g')
mutateChangeOp expr g = (expr, g) -- leaf: no change

-- | Insert a unary node above a random subtree.
mutateInsertNode :: Int -> Int -> Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutateInsertNode _maxSize _nFeatures expr g0 =
    let (opIdx, g1) = randomR (0, length unaryOps - 1) g0
     in (Fix (UnF (unaryOps !! opIdx) expr), g1)

-- | Delete a random intermediate node, replacing it with one of its children.
mutateDeleteNode :: Fix ExprF -> StdGen -> (Fix ExprF, StdGen)
mutateDeleteNode (Fix (UnF _ c)) g = (c, g)
mutateDeleteNode (Fix (BinF _ l r)) g =
    let (coin, g') = randomR (0 :: Int, 1) g
     in if coin == 0 then (l, g') else (r, g')
mutateDeleteNode (Fix (SumF (x : _))) g = (x, g)
mutateDeleteNode (Fix (ProdF (x : _))) g = (x, g)
mutateDeleteNode expr g = (expr, g)

------------------------------------------------------------------------
-- Candidate generation and scoring
------------------------------------------------------------------------

-- | Generate N random candidates + mutations of elite, fit each, return scored list.
generateAndScore ::
    BoostConfig ->
    DataSet ->
    DataSet ->
    PVector ->
    Int ->
    [Fix ExprF] ->
    StdGen ->
    IO ([CandidateScore], StdGen)
generateAndScore cfg (xTrain, _yTrain) (xVal, _yVal) residuals nFeatures elite g0 = do
    let nCands = bcCandidatesPerRound cfg
        maxSz = bcMaxWeakLearnerSize cfg
        -- Generate mutations of elite (3 mutations per elite member)
        (mutants, g1) =
            foldr
                ( \expr (acc, g) ->
                    let (m1, g') = mutateExpr maxSz nFeatures expr g
                        (m2, g'') = mutateExpr maxSz nFeatures expr g'
                        (m3, g''') = mutateExpr maxSz nFeatures expr g''
                     in (m1 : m2 : m3 : acc, g''')
                )
                ([], g0)
                elite
    -- Score mutants first
    (mutantScored, g2) <- goGenFromList mutants g1 []
    -- Then generate fresh random candidates
    goGen nCands maxSz nFeatures g2 mutantScored
  where
    goGenFromList [] g acc = pure (acc, g)
    goGenFromList (raw : rest) g acc = do
        let template = relabelParams raw
            (!fit, !theta, !g') =
                evaluateFitness
                    (bcNloptIters cfg)
                    (bcNloptRetries cfg)
                    (xTrain, residuals)
                    template
                    g
        if fit <= -1e17
            then goGenFromList rest g' acc
            else do
                let !fittedExpr = paramsToConst (VU.toList theta) template
                    !trainVec = evalTree xTrain theta template
                    !valVec = evalTree xVal theta template
                if VU.any isNaN trainVec || VU.any isInfinite trainVec
                    then goGenFromList rest g' acc
                    else do
                        let !rv = VU.sum (VU.zipWith (*) residuals trainVec)
                            !vv = VU.sum (VU.map (\x -> x * x) trainVec)
                            !alpha = if vv > 1e-15 then rv / (vv + 1e-6) else 0
                            !gain = alpha * alpha * vv
                        if isNaN gain || isInfinite gain || gain <= 0
                            then goGenFromList rest g' acc
                            else do
                                let cs =
                                        CandidateScore
                                            { csExpr = fittedExpr
                                            , csTrainVec = trainVec
                                            , csValVec = valVec
                                            , csCoeff = alpha
                                            , csGain = gain
                                            }
                                goGenFromList rest g' (cs : acc)

    goGen 0 _ _ g acc = pure (acc, g)
    goGen !n maxSz nFeat g acc = do
        let (rawExpr, g1) = randomExpr maxSz nFeat g
            template = relabelParams rawExpr
            (!fit, !theta, !g2) =
                evaluateFitness
                    (bcNloptIters cfg)
                    (bcNloptRetries cfg)
                    (xTrain, residuals)
                    template
                    g1
        if fit <= -1e17
            then goGen (n - 1) maxSz nFeat g2 acc
            else do
                let !fittedExpr = paramsToConst (VU.toList theta) template
                    !trainVec = evalTree xTrain theta template
                    !valVec = evalTree xVal theta template
                if VU.any isNaN trainVec || VU.any isInfinite trainVec
                    then goGen (n - 1) maxSz nFeat g2 acc
                    else do
                        let !rv = VU.sum (VU.zipWith (*) residuals trainVec)
                            !vv = VU.sum (VU.map (\x -> x * x) trainVec)
                            !alpha = if vv > 1e-15 then rv / (vv + 1e-6) else 0
                            !gain = alpha * alpha * vv
                        if isNaN gain || isInfinite gain || gain <= 0
                            then goGen (n - 1) maxSz nFeat g2 acc
                            else do
                                let cs =
                                        CandidateScore
                                            { csExpr = fittedExpr
                                            , csTrainVec = trainVec
                                            , csValVec = valVec
                                            , csCoeff = alpha
                                            , csGain = gain
                                            }
                                goGen (n - 1) maxSz nFeat g2 (cs : acc)

------------------------------------------------------------------------
-- Ensemble construction
------------------------------------------------------------------------

-- | Build the full ensemble expression with constants substituted.
buildEnsembleExpr :: Double -> [ActiveTerm] -> Fix ExprF
buildEnsembleExpr bias terms =
    case terms of
        [] -> lit bias
        _ ->
            let scaledTerms = map (\t -> lit (atCoeff t) * atExpr t) terms
             in foldl' (+) (lit bias) scaledTerms

-- | Build an ensemble expression with ParamF coefficients for joint refit.
buildRefitExpr :: Double -> [ActiveTerm] -> Fix ExprF
buildRefitExpr bias terms =
    case terms of
        [] -> lit bias
        _ ->
            let indexedTerms = zipWith (\i t -> param i * atExpr t) [0 ..] terms
             in foldl' (+) (lit bias) indexedTerms

------------------------------------------------------------------------
-- Joint refit and pruning
------------------------------------------------------------------------

-- | Jointly refit all coefficients via ridge regression.
jointRefit ::
    BoostConfig ->
    Features ->
    PVector ->
    Double ->
    [ActiveTerm] ->
    ([ActiveTerm], Double)
jointRefit cfg xTrain yTrain bias terms
    | null terms = ([], bias)
    | otherwise =
        let !ensembleExpr = relabelParams (buildRefitExpr bias terms)
            !theta0 = VU.fromListN (length terms) (map atCoeff terms)
            (!thetaOpt, _, _) =
                minimizeNLLRidge
                    (bcRefitRidge cfg)
                    (bcNloptIters cfg)
                    xTrain
                    yTrain
                    ensembleExpr
                    theta0
            !newTerms =
                zipWith
                    (\t i -> t{atCoeff = thetaOpt `VU.unsafeIndex` i})
                    terms
                    [0 ..]
         in (newTerms, bias)

-- | Prune terms by contribution norm: ||β_j * v_j||₂.
pruneByContribution :: Double -> [ActiveTerm] -> [ActiveTerm]
pruneByContribution threshold terms
    | null terms = []
    | otherwise =
        let contribs = map contribution terms
            maxC = maximum contribs
            cutoff = threshold * maxC
         in [t | (t, c) <- zip terms contribs, c >= cutoff]
  where
    contribution t =
        abs (atCoeff t) * vecNorm (atTrainVec t)

------------------------------------------------------------------------
-- Ensemble collapse via e-graph rewriting
------------------------------------------------------------------------

{- | Collapse the entire ensemble into a single simplified expression.

1. Build ensemble expression
2. Insert into fresh e-graph
3. Saturate with FULL rewrites (including distributivity)
4. Extract simplified expression
5. Refit constants against training targets
6. Replace ensemble with this single term

This is the key mechanism that keeps expressions compact.
-}
collapseEnsemble ::
    BoostConfig ->
    DataSet ->
    DataSet ->
    BoostState ->
    StdGen ->
    IO (BoostState, StdGen)
collapseEnsemble cfg (xTrain, yTrain) (xVal, _yVal) st g0 = do
    let ensemble = buildEnsembleExpr (bsBias st) (bsTerms st)
        rawSize = countNodes ensemble

    -- 1. E-graph saturate with constants
    -- 2. Partial-evaluate: fold all constant subexpressions
    -- 3. Convert to params, e-graph structural pass, refit
    -- Partial-evaluate first: fold all constants, collect like terms.
    -- Then e-graph saturate on the already-simplified expression.
    result <- try $ do
        let !evaled = partialEval ensemble
            !evaledSize = countNodes evaled
        _ <- evaluate evaledSize
        -- Only run e-graph if expression is small enough to avoid blowup
        if evaledSize <= 200
            then do
                let (!cid, !eg1) = insertTree evaled emptySREGraph
                    !eg2 = runSaturationNWith (bcSatIters cfg) RW.srRewrites eg1
                    !simplified = partialEval (getBestExpr cid eg2)
                _ <- evaluate (countNodes simplified)
                pure simplified
            else pure evaled
    case result of
        Left (_ :: SomeException) -> pure (st, g0)
        Right simplified -> do
            let !cleanSize = countNodes simplified
                -- Refit any remaining ParamF slots
                !refit = relabelParams simplified
                (!_fit, !theta, !g1) =
                    evaluateFitness
                        (bcNloptIters cfg)
                        (bcNloptRetries cfg)
                        (xTrain, yTrain)
                        refit
                        g0
                !fittedExpr = partialEval (paramsToConst (VU.toList theta) refit)
                !trainVec = evalTree xTrain theta refit
                !valVec = evalTree xVal theta refit
                !newTerm = ActiveTerm fittedExpr 1.0 trainVec valVec Nothing

            when (bcShowTrace cfg) $
                hPutStrLn stderr $
                    "  Collapse: "
                        ++ show rawSize
                        ++ " → "
                        ++ show cleanSize
                        ++ " nodes, trainMSE="
                        ++ showF (mseScore yTrain trainVec)

            pure
                ( st
                    { bsBias = 0.0
                    , bsTerms = [newTerm]
                    , bsTrainPreds = trainVec
                    , bsValPreds = valVec
                    , bsResiduals = VU.zipWith (-) yTrain trainVec
                    , bsGraph = emptySREGraph
                    }
                , g1
                )

------------------------------------------------------------------------
-- Main loop
------------------------------------------------------------------------

-- | Run the boosting loop.
fitBoosted ::
    StdGen ->
    BoostConfig ->
    DataSet ->
    DataSet ->
    IO BoostResult
fitBoosted g0 cfg trainData@(xTrain, yTrain) valData@(xVal, yVal) = do
    let !nRows = V.length xTrain
        !nFeatures = if nRows == 0 then 0 else VU.length (xTrain `V.unsafeIndex` 0)
        !bias = VU.sum yTrain / fromIntegral nRows
        !trainPreds0 = VU.replicate nRows bias
        !nValRows = V.length xVal
        !valPreds0 = VU.replicate nValRows bias
        !residuals0 = VU.zipWith (-) yTrain trainPreds0

        initState =
            BoostState
                { bsRound = 0
                , bsBias = bias
                , bsTerms = []
                , bsTrainPreds = trainPreds0
                , bsValPreds = valPreds0
                , bsResiduals = residuals0
                , bsGraph = emptySREGraph
                , bsValHistory = []
                , bsElite = []
                }

    (finalState, finalGen) <- boostLoop cfg trainData valData nFeatures initState g0

    let ensembleExpr = buildEnsembleExpr (bsBias finalState) (bsTerms finalState)
        trainMSE = mseScore yTrain (bsTrainPreds finalState)
        valMSE = mseScore yVal (bsValPreds finalState)

    distilled <- distill cfg ensembleExpr xTrain yTrain finalGen

    pure
        BoostResult
            { brEnsembleExpr = ensembleExpr
            , brDistilled = distilled
            , brTerms = bsTerms finalState
            , brTrainMSE = trainMSE
            , brValMSE = valMSE
            }

-- | The main boosting iteration.
boostLoop ::
    BoostConfig ->
    DataSet ->
    DataSet ->
    Int ->
    BoostState ->
    StdGen ->
    IO (BoostState, StdGen)
boostLoop cfg trainData@(xTrain, yTrain) valData@(xVal, yVal) nFeatures st0 g0 = do
    let t = bsRound st0 + 1

    -- Stop conditions
    let trainMSE = mseScore yTrain (bsTrainPreds st0)
    if
        | t > bcRounds cfg -> pure (st0, g0)
        | trainMSE < 1e-12 -> do
            when (bcShowTrace cfg) $
                hPutStrLn stderr $
                    "  Perfect fit at round " ++ show (t - 1) ++ " (MSE < 1e-12)"
            pure (st0, g0)
        | otherwise -> do
            -- Generate random candidates + mutate elite from previous rounds
            (scored, g1) <-
                generateAndScore
                    cfg
                    trainData
                    valData
                    (bsResiduals st0)
                    nFeatures
                    (bsElite st0)
                    g0

            -- Update elite: keep top-10 by gain for mutation in future rounds
            let !newElite =
                    take
                        10
                        [ csExpr cs
                        | cs <- sortBy (comparing (Data.Ord.Down . csGain)) scored
                        ]

            if null scored
                then do
                    when (bcShowTrace cfg) $
                        hPutStrLn stderr $
                            "  Round " ++ show t ++ ": no viable candidates"
                    boostLoop
                        cfg
                        trainData
                        valData
                        nFeatures
                        (st0{bsRound = t, bsGraph = bsGraph st0, bsElite = newElite})
                        g1
                else do
                    let best = maximumBy (comparing csGain) scored

                    let !effCoeff = bcLearningRate cfg * csCoeff best
                        !newTrainPreds =
                            VU.zipWith (+) (bsTrainPreds st0) (VU.map (* effCoeff) (csTrainVec best))
                        !newValPreds =
                            VU.zipWith (+) (bsValPreds st0) (VU.map (* effCoeff) (csValVec best))
                        !newValMSE = mseScore yVal newValPreds
                        !oldValMSE = mseScore yVal (bsValPreds st0)

                    -- Validation gate: don't add term if it makes things worse
                    st1 <-
                        if newValMSE >= oldValMSE
                            then do
                                when (bcShowTrace cfg) $
                                    hPutStrLn stderr $
                                        "  Round " ++ show t ++ ": skipped (val MSE would increase)"
                                pure st0{bsRound = t, bsGraph = bsGraph st0, bsElite = newElite}
                            else do
                                let !newResiduals = VU.zipWith (-) yTrain newTrainPreds
                                    !newTerm =
                                        ActiveTerm
                                            { atExpr = csExpr best
                                            , atCoeff = effCoeff
                                            , atTrainVec = csTrainVec best
                                            , atValVec = csValVec best
                                            , atClassId = Nothing
                                            }
                                    !newTerms = bsTerms st0 ++ [newTerm]

                                when (bcShowTrace cfg) $ do
                                    let tMSE = mseScore yTrain newTrainPreds
                                    hPutStr stderr $
                                        "  Round "
                                            ++ show t
                                            ++ ": terms="
                                            ++ show (length newTerms)
                                            ++ " trainMSE="
                                            ++ showF tMSE
                                            ++ " valMSE="
                                            ++ showF newValMSE
                                            ++ " nodes="
                                            ++ show (countNodes (csExpr best))
                                            ++ "\n"
                                    hFlush stderr

                                pure
                                    st0
                                        { bsRound = t
                                        , bsTerms = newTerms
                                        , bsTrainPreds = newTrainPreds
                                        , bsValPreds = newValPreds
                                        , bsResiduals = newResiduals
                                        , bsGraph = bsGraph st0
                                        , bsElite = newElite
                                        }

                    -- Periodic joint refit + pruning
                    st2 <-
                        if t `mod` bcRefitEvery cfg == 0 && length (bsTerms st1) > 1
                            then do
                                let (refitTerms, newBias) =
                                        jointRefit cfg xTrain yTrain (bsBias st1) (bsTerms st1)
                                    prunedTerms = pruneByContribution (bcPruneThreshold cfg) refitTerms
                                let !newTrainP = recomputePredictions newBias prunedTerms xTrain
                                    !newValP = recomputePredictions newBias prunedTerms xVal
                                    !newRes = VU.zipWith (-) yTrain newTrainP
                                when (bcShowTrace cfg && length prunedTerms < length (bsTerms st1)) $
                                    hPutStrLn stderr $
                                        "  Refit: "
                                            ++ show (length (bsTerms st1))
                                            ++ " → "
                                            ++ show (length prunedTerms)
                                            ++ " terms"
                                pure
                                    st1
                                        { bsBias = newBias
                                        , bsTerms = prunedTerms
                                        , bsTrainPreds = newTrainP
                                        , bsValPreds = newValP
                                        , bsResiduals = newRes
                                        }
                            else pure st1

                    -- Periodic ensemble collapse via full e-graph rewriting
                    (st3, g2) <-
                        if t `mod` bcSimplifyEvery cfg == 0 && not (null (bsTerms st2))
                            then collapseEnsemble cfg trainData valData st2 g1
                            else pure (st2, g1)

                    -- Validation + early stopping
                    let !vMSE = mseScore yVal (bsValPreds st3)
                        !valHist = vMSE : bsValHistory st3
                        !st4 = st3{bsValHistory = valHist}

                    if shouldStop (bcEarlyStopPat cfg) valHist
                        then do
                            when (bcShowTrace cfg) $
                                hPutStrLn stderr $
                                    "  Early stopping at round " ++ show t
                            pure (st4, g2)
                        else boostLoop cfg trainData valData nFeatures st4 g2

-- | Recompute predictions from bias + active terms.
recomputePredictions :: Double -> [ActiveTerm] -> Features -> PVector
recomputePredictions bias terms xss =
    let !nRows = V.length xss
        !base = VU.replicate nRows bias
     in foldl'
            (\acc t -> VU.zipWith (+) acc (VU.map (* atCoeff t) (atTrainVec t)))
            base
            terms

-- | Check if early stopping should trigger.
shouldStop :: Int -> [Double] -> Bool
shouldStop patience history
    | length history < patience + 1 = False
    | otherwise =
        let recent = take patience history
            best = minimum (drop patience history)
         in all (>= best) recent

------------------------------------------------------------------------
-- Distillation
------------------------------------------------------------------------

{- | Distill the ensemble into simplified expression(s).
  Polynomial simplification + e-graph saturation + NLOPT refit.
-}
distill ::
    BoostConfig -> Fix ExprF -> Features -> PVector -> StdGen -> IO [Fix ExprF]
distill _cfg ensembleExpr xTrain yTrain _gen = do
    let !evaled = partialEval ensembleExpr
        (!paramExpr, !_initVals) = constsToParam evaled
        !cleanExpr = relabelParams paramExpr
        !exprSize = countNodes cleanExpr
    result <- try $ do
        if exprSize <= 200
            then do
                let (!cid, !eg1) = insertTree cleanExpr emptySREGraph
                    !eg2 = runSaturationNWith 100 RW.srRewrites eg1
                    !simplified = getBestExpr cid eg2
                _ <- evaluate (countNodes simplified)
                pure simplified
            else pure cleanExpr
    case result of
        Left (_ :: SomeException) -> pure [evaled]
        Right simplified -> do
            let !clean = relabelParams simplified
                !g42 = mkStdGen 42
                (!_fit, !theta, !_g') = evaluateFitness 100 5 (xTrain, yTrain) clean g42
                !final = partialEval (paramsToConst (VU.toList theta) clean)
            pure [final]

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

vecNorm :: PVector -> Double
vecNorm v = sqrt (VU.sum (VU.map (\x -> x * x) v))
{-# INLINE vecNorm #-}

showF :: Double -> String
showF x = let s = show x in take 8 s

------------------------------------------------------------------------
-- Normalize + fold
------------------------------------------------------------------------

{- | Re-insert into a fresh e-graph (which flattens AC chains into
ProdF\/SumF via 'exprNormalize'), extract, then fold constants.
This collects scattered constants in nested Mul\/Add chains into
single constants per chain.
-}
normalizeAndFold :: Fix ExprF -> Fix ExprF
normalizeAndFold expr =
    let !folded = foldConstants expr
        (!cid, !eg) = insertTree folded emptySREGraph
        !flat = getBestExpr cid eg
     in foldConstants flat

------------------------------------------------------------------------
-- Partial evaluator (delegates to Poly normal form)
------------------------------------------------------------------------

{- | Simplify an expression via polynomial normal form.
  Expands polynomial products, collects like monomials, drops near-zeros.
-}
partialEval :: Fix ExprF -> Fix ExprF
partialEval = polySimplify

------------------------------------------------------------------------
-- Constant folding (pure tree pass)
------------------------------------------------------------------------

{- | Fold constant children in ProdF\/SumF and nested BinF chains.

Turns @ProdF [LitF 1.02, LitF 1.01, expr]@ into @ProdF [LitF 1.0302, expr]@
and @SumF [LitF 3.0, LitF 2.0, expr]@ into @SumF [LitF 5.0, expr]@.

Also eliminates identity factors (@1.0 *@) and zero terms (@0.0 +@).
-}
foldConstants :: Fix ExprF -> Fix ExprF
foldConstants = go
  where
    go (Fix node) = case fmap go node of
        ProdF xs ->
            let (consts, nonconsts) = partition isLit xs
                constProd = product [v | Fix (LitF v) <- consts]
             in case nonconsts of
                    [] ->
                        lit constProd
                    [single]
                        | abs (constProd - 1.0) < 1e-12 -> single
                        | abs constProd < 1e-12 -> lit 0
                        | otherwise -> lit constProd * single
                    _
                        | abs constProd < 1e-12 -> lit 0
                        | abs (constProd - 1.0) < 1e-12 -> Fix (ProdF nonconsts)
                        | otherwise -> Fix (ProdF (lit constProd : nonconsts))
        SumF xs ->
            let (consts, nonconsts) = partition isLit xs
                constSum = sum [v | Fix (LitF v) <- consts]
             in case nonconsts of
                    [] ->
                        lit constSum
                    [single]
                        | abs constSum < 1e-12 -> single
                        | otherwise -> single + lit constSum
                    _
                        | abs constSum < 1e-12 -> Fix (SumF nonconsts)
                        | otherwise -> Fix (SumF (lit constSum : nonconsts))
        -- Nested binary constant folding
        BinF Mul (Fix (LitF a)) (Fix (LitF b)) -> lit (a * b)
        BinF Add (Fix (LitF a)) (Fix (LitF b)) -> lit (a + b)
        BinF Sub (Fix (LitF a)) (Fix (LitF b)) -> lit (a - b)
        BinF Div (Fix (LitF a)) (Fix (LitF b))
            | b /= 0 -> lit (a / b)
        -- Identity elimination
        BinF Mul (Fix (LitF 1.0)) r -> r
        BinF Mul l (Fix (LitF 1.0)) -> l
        BinF Mul (Fix (LitF a)) _ | abs a < 1e-12 -> lit 0
        BinF Mul _ (Fix (LitF a)) | abs a < 1e-12 -> lit 0
        BinF Add (Fix (LitF a)) r | abs a < 1e-12 -> r
        BinF Add l (Fix (LitF a)) | abs a < 1e-12 -> l
        -- Unary on literal
        UnF Neg (Fix (LitF a)) -> lit (negate a)
        UnF Abs (Fix (LitF a)) -> lit (abs a)
        UnF Recip (Fix (LitF a)) | a /= 0 -> lit (1 / a)
        UnF Sq (Fix (LitF a)) -> lit (a * a)
        UnF Sqrt (Fix (LitF a)) | a >= 0 -> lit (sqrt a)
        UnF Exp (Fix (LitF a)) -> lit (exp a)
        UnF Log (Fix (LitF a)) | a > 0 -> lit (log a)
        UnF Cos (Fix (LitF a)) -> lit (cos a)
        UnF Sin (Fix (LitF a)) -> lit (sin a)
        other -> Fix other

    isLit (Fix (LitF _)) = True
    isLit _ = False
