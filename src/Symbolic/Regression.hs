{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Symbolic.Regression
Description : Symbolic regression for DataFrames using genetic programming with e-graph optimization

This module provides symbolic regression capabilities for DataFrame workflows.
Given a target column and a dataset, it evolves mathematical expressions that
predict the target variable, returning a Pareto front of expressions trading
off complexity and accuracy.

= Quick Start

@
import qualified DataFrame as D
import DataFrame.Functions ((.=))
import Symbolic.Regression

-- Load your data
df <- D.readParquet "./data/mtcars.parquet"

-- Run symbolic regression to predict 'mpg'
exprs <- fit defaultRegressionConfig mpg df

-- Use the best expression
D.derive "prediction" (last exprs) df
@

= Important Notes

All columns used in regression must be converted to 'Double' first.
Symbolic regression will by default only use the double columns.

= How It Works

1. __Genetic Programming__: Evolves a population of expression trees through
   selection, crossover, and mutation
2. __E-graph Optimization__: Uses equality saturation to discover equivalent
   expressions and simplify
3. __Parameter Optimization__: Fits numerical constants using nonlinear optimization
4. __Pareto Selection__: Returns expressions across the complexity-accuracy frontier
-}
module Symbolic.Regression (
    -- * Main API
    fit,

    -- * Configuration
    RegressionConfig (..),
    ValidationConfig (..),
    defaultRegressionConfig,
) where

import Control.Exception (throw)
import Control.Monad.State.Strict
import Data.Massiv.Array as MA hiding (forM, forM_)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Internal.Expression
import System.Random

import Algorithm.EqSat.Build
import Algorithm.EqSat.DB
import Algorithm.EqSat.Egraph
import Algorithm.EqSat.Info
import Algorithm.EqSat.Queries
import Algorithm.EqSat.Simplify hiding (myCost)
import Algorithm.SRTree.Likelihoods
import Algorithm.SRTree.ModelSelection (fractionalBayesFactor)
import Control.Lens (over)
import Control.Monad (
    filterM,
    forM,
    forM_,
    replicateM,
    unless,
    when,
    (>=>),
 )
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Function (on)
import Data.Functor
import qualified Data.HashSet as Set
import qualified Data.IntMap.Strict as IM
import Data.List (
    intercalate,
    maximumBy,
    nub,
    zip4,
 )
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.SRTree
import Data.SRTree.Datasets
import qualified Data.SRTree.Internal as SI
import Data.SRTree.Print
import Data.SRTree.Random
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (Refl))
import Type.Reflection (typeRep)

import Algorithm.EqSat (runEqSat)
import Algorithm.EqSat.SearchSR
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import Text.ParseSR

{- | Configuration for the symbolic regression algorithm.

Use 'defaultRegressionConfig' as a starting point and modify fields as needed:

@
myConfig :: RegressionConfig
myConfig = defaultRegressionConfig
    { generations = 200
    , maxExpressionSize = 7
    , populationSize = 200
    }
@
-}
data RegressionConfig = RegressionConfig
    { generations :: Int
    -- ^ Number of evolutionary generations to run (default: 100)
    , maxExpressionSize :: Int
    -- ^ Maximum tree depth\/complexity for generated expressions (default: 5)
    , validationConfig :: Maybe ValidationConfig
    -- ^ The configuration for cross validation.
    , showTrace :: Bool
    -- ^ Whether to print progress during evolution (default: 'True')
    , lossFunction :: Distribution
    -- ^ Loss function to optimize: 'MSE', 'Gaussian', 'Poisson', etc. (default: 'MSE')
    , numOptimisationIterations :: Int
    -- ^ Number of iterations for parameter optimization (default: 30)
    , numParameterRetries :: Int
    -- ^ Number of retries for parameter fitting (default: 2)
    , populationSize :: Int
    -- ^ Size of the expression population (default: 100)
    , tournamentSize :: Int
    -- ^ Number of individuals in tournament selection (default: 3)
    , crossoverProbability :: Double
    -- ^ Probability of crossover between expressions (default: 0.95)
    , mutationProbability :: Double
    -- ^ Probability of mutation (default: 0.3)
    , unaryFunctions :: [D.Expr Double -> D.Expr Double]
    -- ^ Unary operations to include in the search space (default: @[]@)
    , binaryFunctions :: [D.Expr Double -> D.Expr Double -> D.Expr Double]
    {- ^ Binary operations to include in the search space
    (default: @[(+), (-), (*), (\/)]@)
    -}
    , numParams :: Int
    -- ^ Number of parameters to use. Set to @-1@ for automatic detection (default: -1)
    , generational :: Bool
    -- ^ Whether to use generational replacement strategy (default: 'False')
    , simplifyExpressions :: Bool
    -- ^ Whether to simplify output expressions using e-graph optimization (default: 'True')
    , maxTime :: Int
    -- ^ Time limit in seconds. Set to @-1@ for no limit (default: -1)
    , dumpTo :: String
    -- ^ File path to save e-graph state for later resumption (default: @\"\"@)
    , loadFrom :: String
    -- ^ File path to load e-graph state from a previous run (default: @\"\"@)
    }

data ValidationConfig = ValidationConfig
    { validationPercent :: Double
    , validationSeed :: Int
    }

-- | Type alias for the multi-view fitness function used throughout the algorithm.
type FitFun = Fix SRTree -> RndEGraph (Double, [PVector])

-- | Shared environment threaded through all egraphGP helpers.
data EGPEnv = EGPEnv
    { envCfg :: RegressionConfig
    , envFitFun :: FitFun
    , envTerms :: [Fix SRTree]
    , envParams :: [Fix SRTree]
    , envNonTerms :: [SRTree ()]
    , envUniNonTerms :: [SRTree ()]
    , envBinNonTerms :: [SRTree ()]
    , envShouldReparam :: Bool
    , envRelabel :: Fix SRTree -> Fix SRTree
    , envVarnames :: String
    , envDataTrainVals :: [(DataSet, DataSet)]
    , envDataTests :: [DataSet]
    }

{- | Default configuration for symbolic regression.

Provides sensible defaults for most use cases:

* 100 generations with population size 100
* Maximum expression size of 5
* 3-fold cross-validation
* MSE loss function
* Basic arithmetic operations: @+@, @-@, @*@, @\/@

Modify specific fields to customize the search behavior.
-}
defaultRegressionConfig :: RegressionConfig
defaultRegressionConfig =
    RegressionConfig
        { generations = 100
        , maxExpressionSize = 5
        , validationConfig = Nothing
        , showTrace = True
        , lossFunction = MSE
        , numOptimisationIterations = 30
        , numParameterRetries = 2
        , populationSize = 100
        , tournamentSize = 3
        , crossoverProbability = 0.95
        , mutationProbability = 0.3
        , numParams = -1
        , generational = False
        , simplifyExpressions = True
        , maxTime = -1
        , unaryFunctions = [(`F.pow` 2), (`F.pow` 3), log, (1 /)]
        , binaryFunctions = [(+), (-), (*), (/)]
        , dumpTo = ""
        , loadFrom = ""
        }

-- Maximum e-node count before compaction is triggered.
maxMem :: Int
maxMem = 2000000

-- | True for unary SRTree nodes.
isUni :: SRTree a -> Bool
isUni (Uni _ _) = True
isUni _ = False

-- | True for binary SRTree nodes.
isBin :: SRTree a -> Bool
isBin (Bin{}) = True
isBin _ = False

-- | Split DataFrame into train/validation sets; both equal to the full data when no validation config is set.
splitTrainVal :: RegressionConfig -> D.DataFrame -> (D.DataFrame, D.DataFrame)
splitTrainVal cfg df = case validationConfig cfg of
    Nothing -> (df, df)
    Just vcfg ->
        D.randomSplit (mkStdGen (validationSeed vcfg)) (1 - validationPercent vcfg) df

-- | Names of all Double-typed columns, excluding the target.
doubleFeatureCols :: D.Expr Double -> D.DataFrame -> [T.Text]
doubleFeatureCols tgt d =
    D.columnNames
        (D.exclude [F.name tgt] (D.selectBy [D.byProperty (D.hasElemType @Double)] d))

-- | Feature matrix as row vectors of Doubles, excluding the target column.
toFeatureMatrix :: D.Expr Double -> D.DataFrame -> V.Vector (VU.Vector Double)
toFeatureMatrix tgt d =
    either throw id $
        D.toDoubleMatrix
            (D.exclude [F.name tgt] (D.selectBy [D.byProperty (D.hasElemType @Double)] d))

-- | Comma-separated non-terminal symbol names derived from the configured operations.
buildNonterminals :: RegressionConfig -> String
buildNonterminals cfg =
    intercalate "," $
        Prelude.map
            (toNonTerminal . (\f -> f (F.col "fake1") (F.col "fake2")))
            (binaryFunctions cfg)
            ++ Prelude.map (toNonTerminal . (\f -> f (F.col "fake1"))) (unaryFunctions cfg)

-- | Comma-separated variable names from a list of column name texts.
buildVarnames :: [T.Text] -> String
buildVarnames cols = intercalate "," (Prelude.map T.unpack cols)

{- | Run symbolic regression to discover mathematical expressions that fit the data.

Returns a list of expressions representing the Pareto front, ordered by
complexity (simplest first). Each expression:

* Is a valid @'D.Expr' 'Double'@ that can be used with DataFrame operations
* Represents a different trade-off between simplicity and accuracy
* Has optimized numerical constants

= Example

@
exprs <- fit defaultRegressionConfig targetColumn df

-- View discovered expressions
map D.prettyPrint exprs
-- [\"qsec\", \"57.33 \/ wt\", \"10.75 + (1557.67 \/ disp)\"]

-- Use expressions in DataFrame operations
D.derive \"prediction\" (last exprs) df
@

= Important

All columns must be converted to 'Double' before running regression.
The algorithm will only use double-typed columns as features.
-}
fit ::
    -- | Random generator for reproducible runs
    StdGen ->
    -- | Configuration controlling the search algorithm
    RegressionConfig ->
    -- | Target column expression to predict
    D.Expr Double ->
    -- | Input DataFrame containing features and target
    D.DataFrame ->
    -- | Pareto front of expressions, ordered simplest to most complex
    IO [D.Expr Double]
fit g cfg targetColumn df =
    let (train, validation) = splitTrainVal cfg df
        cols = doubleFeatureCols targetColumn train
        fm d =
            fromLists' Seq (V.toList (V.map VU.toList (toFeatureMatrix targetColumn d))) ::
                Array S Ix2 Double
        tgt d = fromLists' Seq (D.columnAsList targetColumn d) :: Array S Ix1 Double
        nts = buildNonterminals cfg
        vars = buildVarnames cols
        alg =
            evalStateT
                ( egraphGP
                    cfg
                    nts
                    vars
                    [((fm train, tgt train, Nothing), (fm validation, tgt validation, Nothing))]
                    [(fm df, tgt df, Nothing)]
                )
                emptyGraph
     in fmap (Prelude.map (toExpr cols)) (evalStateT alg g)

-- | Convert a unary SRTree operation to the equivalent DataFrame expression.
toExprUni :: [T.Text] -> SI.Function -> Fix SRTree -> Expr Double
toExprUni cols SI.Square v = F.pow (toExpr cols v) 2
toExprUni cols SI.Cube v = F.pow (toExpr cols v) 3
toExprUni cols SI.Log v = log (toExpr cols v)
toExprUni cols SI.Recip v = F.lit 1 / toExpr cols v
toExprUni _ treeOp _ = error ("UNIMPLEMENTED OPERATION: " ++ show treeOp)

-- | Convert a binary SRTree operation to the equivalent DataFrame expression.
toExprBin :: [T.Text] -> SI.Op -> Fix SRTree -> Fix SRTree -> Expr Double
toExprBin cols SI.Add l r = toExpr cols l + toExpr cols r
toExprBin cols SI.Sub l r = toExpr cols l - toExpr cols r
toExprBin cols SI.Mul l r = toExpr cols l * toExpr cols r
toExprBin cols SI.Div l r = toExpr cols l / toExpr cols r
toExprBin _ treeOp _ _ = error ("UNIMPLEMENTED OPERATION: " ++ show treeOp)

-- | Recursively convert a fixed-point SRTree to a DataFrame expression.
toExpr :: [T.Text] -> Fix SRTree -> Expr Double
toExpr _ (Fix (Const v)) = Lit v
toExpr cols (Fix (Var ix)) = Col (cols !! ix)
toExpr cols (Fix (Uni f v)) = toExprUni cols f v
toExpr cols (Fix (Bin op l r)) = toExprBin cols op l r
toExpr _ _ = error "UNIMPLEMENTED"

-- | Map a DataFrame expression node to its e-graph non-terminal name (e.g. "add", "square").
toNonTerminal :: D.Expr Double -> String
toNonTerminal e = case e of
    Binary op left right -> handleBin (T.unpack $ binaryName op) left right
    Unary op _ -> handleUni (T.unpack $ unaryName op)
    _ -> error ("Unsupported operation: " ++ show e)
  where
    handleBin name left right
        | name == "add" = "add"
        | name == "sub" = "sub"
        | name == "mult" = "mul"
        | name == "divide" = handleDiv left
        | name == "pow" = handlePow right
        | otherwise = error ("Unsupported binary: " ++ name)
    handleUni name
        | name == "log" = "log"
        | otherwise = error ("Unsupported unary: " ++ name)
    handleDiv (Lit n :: Expr b) = case testEquality (typeRep @b) (typeRep @Double) of
        Nothing -> error "[Internal Error] - Reciprocal of non-double"
        Just Refl -> case n of 1 -> "recip"; _ -> error "Unknown reciprocal"
    handleDiv _ = "div"
    handlePow (ex :: Expr b) =
        ( case testEquality (typeRep @b) (typeRep @Int) of
            Nothing -> error "Impossible: Raised to non-int power"
            Just Refl -> case ex of
                Lit 2 -> "square"
                Lit 3 -> "cube"
                _ -> error "Unknown power"
        ) ::
            String

-- | Compute NLL, R², and fractional Bayes factor across train, validation, and test sets.
computeDatasetMetrics ::
    Fix SRTree ->
    Distribution ->
    Double ->
    Array S Ix1 Double ->
    DataSet ->
    DataSet ->
    DataSet ->
    String
computeDatasetMetrics best' dist maxLoss theta' (x, y, mYErr) (xv, yv, mYErrV) (xt, yt, mYErrT) =
    let showNA z = if isNaN z then "" else show z
        nlls =
            [ nll dist mYErr x y best' theta'
            , nll dist mYErrV xv yv best' theta'
            , nll dist mYErrT xt yt best' theta'
            ]
        r2s = [r2 x y best' theta', r2 xv yv best' theta', r2 xt yt best' theta']
        mdls =
            [ fractionalBayesFactor dist mYErr x y theta' best'
            , fractionalBayesFactor dist mYErrV xv yv theta' best'
            , fractionalBayesFactor dist mYErrT xt yt theta' best'
            ]
     in intercalate "," $ Prelude.map showNA (nlls ++ [maxLoss] ++ r2s ++ mdls)

-- | Compute terminal and parameter lists from config and training data.
mkEnvTerminals ::
    RegressionConfig -> [(DataSet, DataSet)] -> ([Fix SRTree], [Fix SRTree])
mkEnvTerminals cfg dataTrainVals =
    let (Sz2 _ nFeats) = case dataTrainVals of [] -> Sz2 0 0; (h : _) -> MA.size (getX . fst $ h)
        ps =
            if numParams cfg == -1
                then [param 0]
                else Prelude.map param [0 .. numParams cfg - 1]
        ts =
            if lossFunction cfg == ROXY
                then var 0 : ps
                else [var ix | ix <- [0 .. nFeats - 1]]
     in (ts, ps)

-- | Build the shared environment for egraphGP helpers.
mkEGPEnv ::
    RegressionConfig ->
    String ->
    String ->
    [(DataSet, DataSet)] ->
    [DataSet] ->
    EGPEnv
mkEGPEnv cfg nonterminals varnames dataTrainVals dataTests =
    let (terms, params) = mkEnvTerminals cfg dataTrainVals
        nonTerms = parseNonTerms nonterminals
        shouldReparam = numParams cfg == -1
     in EGPEnv
            { envCfg = cfg
            , envShouldReparam = shouldReparam
            , envVarnames = varnames
            , envFitFun =
                fitnessMV
                    shouldReparam
                    (numParameterRetries cfg)
                    (numOptimisationIterations cfg)
                    (lossFunction cfg)
                    dataTrainVals
            , envTerms = terms
            , envParams = params
            , envNonTerms = nonTerms
            , envUniNonTerms = [t | t <- nonTerms, isUni t]
            , envBinNonTerms = [t | t <- nonTerms, isBin t]
            , envRelabel = if shouldReparam then relabelParams else relabelParamsOrder
            , envDataTrainVals = dataTrainVals
            , envDataTests = dataTests
            }

-- | Strip a node's children to produce an arity template for mutation candidate matching.
peel :: Fix SRTree -> SRTree ()
peel (Fix (Bin op _ _)) = Bin op () ()
peel (Fix (Uni f _)) = Uni f ()
peel (Fix (Param ix)) = Param ix
peel (Fix (Var ix)) = Var ix
peel (Fix (Const x)) = Const x

-- | Collect all sub-expression e-class IDs from a tree (crossover candidate pool).
getAllSubClasses :: EClassId -> RndEGraph [EClassId]
getAllSubClasses p' = do
    p <- canonical p'
    en <- getBestENode p
    case en of
        Bin _ l r -> do
            ls <- getAllSubClasses l
            rs <- getAllSubClasses r
            pure (p : ls <> rs)
        Uni _ t -> (p :) <$> getAllSubClasses t
        _ -> pure [p]

-- | Run step function f for n iterations, halting early if the time budget expires.
iterateFor ::
    Int ->
    POSIXTime ->
    Maybe NominalDiffTime ->
    a ->
    (Int -> a -> RndEGraph a) ->
    RndEGraph a
iterateFor 0 _ _ xs _ = pure xs
iterateFor n t0' maxT xs f = do
    xs' <- f n xs
    t1 <- io getPOSIXTime
    let maxT' = subtract (t1 - t0') <$> maxT
    case maxT' of
        Just mt | mt <= 0 -> pure xs
        _ -> iterateFor (n - 1) t1 maxT' xs' f

-- | Wall-clock budget in seconds, or Nothing for unlimited.
maxTimeMaybe :: EGPEnv -> Maybe NominalDiffTime
maxTimeMaybe env =
    if maxTime (envCfg env) < 0
        then Nothing
        else Just (fromIntegral $ maxTime (envCfg env) - 5)

-- | Restore e-graph state from disk if loadFrom path is set.
loadState :: EGPEnv -> RndEGraph ()
loadState env =
    unless (null (loadFrom (envCfg env))) $
        io (BS.readFile (loadFrom (envCfg env))) >>= \eg -> put (decode eg)

-- | Persist e-graph state to disk if dumpTo path is set.
saveState :: EGPEnv -> RndEGraph ()
saveState env =
    unless (null (dumpTo (envCfg env))) $
        get >>= (io . BS.writeFile (dumpTo (envCfg env)) . encode)

-- | Pre-populate the e-graph with all terminal symbols (variables and params).
insertTerms :: EGPEnv -> RndEGraph [EClassId]
insertTerms env = forM (envTerms env) (fromTree myCost >=> canonical)

-- | Sample a random terminal: variable, constant, or learnable parameter.
rndTerm :: EGPEnv -> Rng IO (Fix SRTree)
rndTerm env = do
    coin <- toss
    if coin || numParams (envCfg env) == 0
        then randomFrom (envTerms env)
        else randomFrom (envParams env)

-- | Sample a random non-terminal (operation node).
rndNonTerm :: EGPEnv -> Rng IO (SRTree ())
rndNonTerm env = randomFrom (envNonTerms env)

-- | Re-evaluate fitness for every e-class marked dirty by e-graph rewrites.
refitChanged :: EGPEnv -> RndEGraph ()
refitChanged env = do
    ids <-
        (gets (_refits . _eDB) >>= Prelude.mapM canonical . Set.toList)
            Data.Functor.<&> nub
    modify' $ over (eDB . refits) (const Set.empty)
    forM_ ids $ \ec -> do
        t <- getBestExpr ec
        (f, p) <- envFitFun env t
        insertFitness ec f p

-- | Generate the initial random population and evaluate fitness for each member.
initPopulation :: EGPEnv -> RndEGraph [EClassId]
initPopulation env = replicateM (populationSize (envCfg env)) $ do
    ec <-
        insertRndExpr (maxExpressionSize (envCfg env)) (rndTerm env) (rndNonTerm env)
            >>= canonical
    _ <- updateIfNothing (envFitFun env) ec
    pure ec

-- | Emit formatted expression rows for all indexed members when tracing is on.
tracePopulation :: EGPEnv -> [(Int, EClassId)] -> RndEGraph [[String]]
tracePopulation env indexed =
    if showTrace (envCfg env)
        then forM indexed (uncurry (printExpr' env))
        else pure []

-- | Combine Pareto-front elites with remaining offspring to form next population.
selectNewPop :: EGPEnv -> Bool -> [EClassId] -> RndEGraph [EClassId]
selectNewPop env full newPop' = do
    pareto <-
        concat
            <$> forM [1 .. maxExpressionSize (envCfg env)] (`getTopFitEClassWithSize` 2)
    let remainder = populationSize (envCfg env) - length pareto
    lft <-
        if full
            then getTopFitEClassThat remainder (const True)
            else pure $ Prelude.take remainder newPop'
    Prelude.mapM canonical (pareto <> lft)

-- | Compact e-graph by discarding all nodes except the Pareto-front expressions.
cleanEGraph :: EGPEnv -> RndEGraph ()
cleanEGraph env = do
    io . putStrLn $ "cleaning"
    pareto <-
        forM [1 .. maxExpressionSize (envCfg env)] (`getTopFitEClassWithSize` 10)
            >>= Prelude.mapM canonical . concat
    infos <- forM pareto (\c -> gets (fmap _info . (IM.!? c) . _eClass))
    exprs <- forM pareto getBestExpr
    put emptyGraph
    newIds <- fromTrees myCost $ Prelude.map (envRelabel env) exprs
    let restore (eId, Just i'') = insertFitness eId (fromJust $ _fitness i'') (_theta i'')
        restore (_, Nothing) = pure ()
    forM_ (Prelude.zip newIds (Prelude.reverse infos)) restore

-- | Run one tournament over challengers; return the fittest.
applyTournament :: EGPEnv -> [EClassId] -> RndEGraph EClassId
applyTournament env xs = do
    challengers <-
        replicateM (tournamentSize (envCfg env)) (rnd $ randomFrom xs)
            >>= traverse canonical
    fits <- Prelude.map fromJust <$> Prelude.mapM getFitness challengers
    pure . snd . maximumBy (compare `on` fst) $ Prelude.zip fits challengers

-- | Select two parents by running independent tournaments.
tournament :: EGPEnv -> [EClassId] -> RndEGraph (EClassId, EClassId)
tournament env xs = do
    p1 <- applyTournament env xs >>= canonical
    p2 <- applyTournament env xs >>= canonical
    pure (p1, p2)

-- | Apply crossover then mutation to a parent pair.
combine :: EGPEnv -> (EClassId, EClassId) -> RndEGraph EClassId
combine env (p1, p2) = crossover env p1 p2 >>= mutate env >>= canonical

-- | Descend into the right child when the crossover position lies past the left subtree.
getSubtreeBinRight ::
    EGPEnv ->
    Int ->
    Int ->
    Op ->
    EClassId ->
    EClassId ->
    Int ->
    Maybe (EClassId -> ENode) ->
    [Maybe (EClassId -> ENode)] ->
    [EClassId] ->
    RndEGraph (Fix SRTree)
getSubtreeBinRight env pos sz op l r szLft parent mGrandParents cands = do
    l' <- getBestExpr l
    r' <-
        getSubtree
            env
            (pos - szLft - 1)
            (sz + szLft + 1)
            (Just $ Bin op l)
            (parent : mGrandParents)
            cands
            r
    pure . Fix $ Bin op l' r'

-- | Descend into the left child when the crossover position lies within it.
getSubtreeBinLeft ::
    EGPEnv ->
    Int ->
    Int ->
    Op ->
    EClassId ->
    EClassId ->
    Int ->
    Maybe (EClassId -> ENode) ->
    [Maybe (EClassId -> ENode)] ->
    [EClassId] ->
    RndEGraph (Fix SRTree)
getSubtreeBinLeft env pos sz op l r szRgt parent mGrandParents cands = do
    l' <-
        getSubtree
            env
            (pos - 1)
            (sz + szRgt + 1)
            (Just (\t -> Bin op t r))
            (parent : mGrandParents)
            cands
            l
    r' <- getBestExpr r
    pure . Fix $ Bin op l' r'

-- | Walk to position pos, then swap that subtree with a size-safe candidate from the pool.
getSubtree ::
    EGPEnv ->
    Int ->
    Int ->
    Maybe (EClassId -> ENode) ->
    [Maybe (EClassId -> ENode)] ->
    [EClassId] ->
    EClassId ->
    RndEGraph (Fix SRTree)
getSubtree env 0 sz (Just parent) mGrandParents cands p' = do
    p <- canonical p'
    candidates <-
        filterM (fmap (< maxExpressionSize (envCfg env) - sz) . getSize) cands
            >>= filterM (doesNotExistGens mGrandParents . parent)
            >>= traverse canonical
    if null candidates
        then getBestExpr p
        else rnd (randomFrom candidates) >>= getBestExpr
getSubtree env pos sz parent mGrandParents cands p' = do
    p <- canonical p'
    root <- getBestENode p >>= canonize
    case root of
        Param ix -> pure . Fix $ Param ix
        Const x -> pure . Fix $ Const x
        Var ix -> pure . Fix $ Var ix
        Uni f t' ->
            canonical t'
                >>= fmap (Fix . Uni f)
                    . getSubtree env (pos - 1) (sz + 1) (Just $ Uni f) (parent : mGrandParents) cands
        Bin op l'' r'' -> do
            l <- canonical l''
            r <- canonical r''
            szLft <- getSize l
            szRgt <- getSize r
            if szLft < pos
                then getSubtreeBinRight env pos sz op l r szLft parent mGrandParents cands
                else getSubtreeBinLeft env pos sz op l r szRgt parent mGrandParents cands

-- | Subtree crossover: swap a random subtree of p1 with a size-compatible subtree from p2.
crossover :: EGPEnv -> EClassId -> EClassId -> RndEGraph EClassId
crossover env p1 p2 = do
    sz <- getSize p1
    coin <- rnd $ tossBiased (crossoverProbability (envCfg env))
    if sz == 1 || not coin
        then rnd (randomFrom [p1, p2])
        else do
            pos <- rnd $ randomRange (1, sz - 1)
            cands <- getAllSubClasses p2
            tree <- getSubtree env pos 0 Nothing [] cands p1
            fromTree myCost (envRelabel env tree) >>= canonical

-- | Randomly replace one subtree; no-op with probability (1 - mutationProbability).
mutate :: EGPEnv -> EClassId -> RndEGraph EClassId
mutate env p = do
    sz <- getSize p
    coin <- rnd $ tossBiased (mutationProbability (envCfg env))
    if coin
        then do
            pos <- rnd $ randomRange (0, sz - 1)
            tree <- mutAt env pos (maxExpressionSize (envCfg env)) Nothing p
            fromTree myCost (envRelabel env tree) >>= canonical
        else pure p

-- | All structurally compatible replacement nodes for the given parent and node arity.
candidatesFor :: EGPEnv -> (EClassId -> ENode) -> ENode -> RndEGraph [SRTree ()]
candidatesFor env parent root = case length (childrenOf root) of
    0 ->
        filterM
            (checkToken parent . replaceChildren (childrenOf root))
            (Prelude.map peel (envTerms env))
    1 ->
        filterM
            (checkToken parent . replaceChildren (childrenOf root))
            (envUniNonTerms env)
    2 ->
        filterM
            (checkToken parent . replaceChildren (childrenOf root))
            (envBinNonTerms env)
    _ -> pure []

-- | Mutation at the target position with a parent context: generate and validate replacement.
mutAtZeroJust :: EGPEnv -> Int -> (EClassId -> ENode) -> RndEGraph (Fix SRTree)
mutAtZeroJust env sizeLeft parent = do
    ec <- insertRndExpr sizeLeft (rndTerm env) (rndNonTerm env) >>= canonical
    (Fix tree) <- getBestExpr ec
    root <- getBestENode ec
    exist <- canonize (parent ec) >>= doesExist
    if not exist
        then pure (Fix tree)
        else do
            candidates <- candidatesFor env parent root
            if null candidates
                then pure (Fix tree)
                else
                    rnd (randomFrom candidates) >>= \t -> pure . Fix $ replaceChildren (childrenOf tree) t

-- | Route mutation recursion into the correct child of a binary node.
mutAtBin ::
    EGPEnv -> Int -> Int -> Op -> EClassId -> EClassId -> RndEGraph (Fix SRTree)
mutAtBin env pos sizeLeft op l r = do
    szLft <- getSize l
    szRgt <- getSize r
    if szLft < pos
        then do
            l' <- getBestExpr l
            r' <- mutAt env (pos - szLft - 1) (sizeLeft - szLft - 1) (Just $ Bin op l) r
            pure . Fix $ Bin op l' r'
        else do
            l' <- mutAt env (pos - 1) (sizeLeft - szRgt - 1) (Just (\t -> Bin op t r)) l
            r' <- getBestExpr r
            pure . Fix $ Bin op l' r'

-- | Replace the subtree at position pos with a new random subtree within size budget.
mutAt ::
    EGPEnv ->
    Int ->
    Int ->
    Maybe (EClassId -> ENode) ->
    EClassId ->
    RndEGraph (Fix SRTree)
mutAt env 0 sizeLeft Nothing _ =
    insertRndExpr sizeLeft (rndTerm env) (rndNonTerm env)
        >>= canonical
        >>= getBestExpr
mutAt env 0 1 _ _ = rnd $ randomFrom (envTerms env)
mutAt env 0 sizeLeft (Just p) _ = mutAtZeroJust env sizeLeft p
mutAt env pos sizeLeft _ p' = do
    p <- canonical p'
    root <- getBestENode p >>= canonize
    case root of
        Param ix -> pure . Fix $ Param ix
        Const x -> pure . Fix $ Const x
        Var ix -> pure . Fix $ Var ix
        Uni f t' ->
            canonical t'
                >>= fmap (Fix . Uni f) . mutAt env (pos - 1) (sizeLeft - 1) (Just $ Uni f)
        Bin op ln rn ->
            canonical ln >>= \l -> canonical rn >>= \r -> mutAtBin env pos sizeLeft op l r

-- | Produce one offspring via tournament selection, crossover, mutation, and e-graph rewriting.
evolve :: EGPEnv -> [EClassId] -> RndEGraph EClassId
evolve env xs' = do
    xs <- Prelude.mapM canonical xs'
    parents' <- tournament env xs
    offspring <- combine env parents'
    if numParams (envCfg env) == 0
        then runEqSat myCost rewritesWithConstant 1 >> cleanDB >> refitChanged env
        else runEqSat myCost rewritesParams 1 >> cleanDB >> refitChanged env
    canonical offspring >>= updateIfNothing (envFitFun env) >> pure ()
    canonical offspring

-- | Execute one generation: produce offspring, compact if needed, select survivors.
runGeneration ::
    EGPEnv ->
    Int ->
    ([EClassId], [[String]], Int) ->
    RndEGraph ([EClassId], [[String]], Int)
runGeneration env _ (ps', out, curIx) = do
    newPop' <- replicateM (populationSize (envCfg env)) (evolve env ps')
    out' <- tracePopulation env (Prelude.zip [curIx ..] newPop')
    totSz <- gets (Map.size . _eNodeToEClass)
    let full = totSz > max maxMem (populationSize (envCfg env))
    when full (cleanEGraph env >> cleanDB)
    newPop <-
        if generational (envCfg env)
            then Prelude.mapM canonical newPop'
            else selectNewPop env full newPop'
    pure (newPop, out <> out', curIx + populationSize (envCfg env))

-- | Best expression for an e-class, optionally simplified, with params relabeled.
getBestRelabeled :: EGPEnv -> EClassId -> RndEGraph (Fix SRTree)
getBestRelabeled env ec = do
    bestExpr <-
        (if simplifyExpressions (envCfg env) then simplifyEqSatDefault else id)
            <$> getBestExpr ec
    pure $
        if envShouldReparam env
            then relabelParams bestExpr
            else relabelParamsOrder bestExpr

-- | Re-optimize parameters when their count has changed since last evaluation.
refitIfNeeded :: EGPEnv -> Fix SRTree -> Maybe [PVector] -> RndEGraph [PVector]
refitIfNeeded env best' thetas' = do
    let fromSz (MA.Sz x) = x
        nParams' = countParamsUniq best'
        nThetas = fmap (Prelude.map (fromSz . MA.size)) thetas'
    if maybe False (Prelude.any (/= nParams')) nThetas
        then snd <$> envFitFun env best'
        else pure (fromMaybe [] thetas')

-- | Format one CSV row: index, view, expression (text/Python/LaTeX), params, metrics.
formatRow ::
    EGPEnv -> Int -> Int -> Fix SRTree -> Fix SRTree -> String -> String -> String
formatRow env ix view expr best' thetaStr vals =
    let vn = envVarnames env
        showExprFun = if null vn then showExpr else showExprWithVars (splitOn "," vn)
        showLatexFun = if null vn then showLatex else showLatexWithVars (splitOn "," vn)
     in show ix
            <> ","
            <> show view
            <> ","
            <> showExprFun expr
            <> ",\""
            <> showPython best'
            <> "\",\"$$"
            <> showLatexFun best'
            <> "$$\","
            <> thetaStr
            <> ","
            <> show @Int (countNodes $ convertProtectedOps expr)
            <> ","
            <> vals

-- | Emit formatted output rows for an e-class across all data views.
printExpr' :: EGPEnv -> Int -> EClassId -> RndEGraph [String]
printExpr' env ix ec' = do
    ec <- canonical ec'
    thetas' <- gets (fmap (_theta . _info) . (IM.!? ec) . _eClass)
    best' <- getBestRelabeled env ec
    thetas <- refitIfNeeded env best' thetas'
    maxLoss <- negate . fromJust <$> getFitness ec
    let dist = lossFunction (envCfg env)
    forM
        (Data.List.zip4 [(0 :: Int) ..] (envDataTrainVals env) (envDataTests env) thetas) $
        \(view, (dataTrain, dataVal), dataTest, theta') ->
            let expr = paramsToConst (MA.toList theta') best'
                thetaStr = intercalate ";" $ Prelude.map show (MA.toList theta')
                vals =
                    computeDatasetMetrics best' dist maxLoss theta' dataTrain dataVal dataTest
             in pure $ formatRow env ix view expr best' thetaStr vals

-- | Best expression, optionally simplified, with params relabeled (used for Pareto front).
extractBestExpr :: EGPEnv -> EClassId -> RndEGraph (Fix SRTree)
extractBestExpr env ec =
    relabelParams
        . (if simplifyExpressions (envCfg env) then simplifyEqSatDefault else id)
        <$> getBestExpr ec

-- | Record this e-class on the Pareto front and recurse to the next complexity tier.
tryImprove ::
    EGPEnv ->
    EClassId ->
    Double ->
    Double ->
    Int ->
    (Int -> Double -> RndEGraph [Fix SRTree]) ->
    RndEGraph [Fix SRTree]
tryImprove env ec f' f n goFn = do
    thetas' <- gets (fmap (_theta . _info) . (IM.!? ec) . _eClass)
    bestExpr <- extractBestExpr env ec
    let t = case thetas' of
            Just (h : _) -> paramsToConst (MA.toList h) bestExpr
            _ -> Fix (Const 0)
    ts <- goFn (n + 1) (max f f')
    pure (t : ts)

-- | Extract Pareto front: best expression per complexity level with strictly improving fitness.
paretoFront' :: EGPEnv -> Int -> RndEGraph [Fix SRTree]
paretoFront' env maxSize' = go 1 (-(1.0 / 0.0))
  where
    go :: Int -> Double -> RndEGraph [Fix SRTree]
    go n f
        | n > maxSize' = pure []
        | otherwise = do
            ecList <- getBestExprWithSize n
            case ecList of
                [] -> go (n + 1) f
                (ec', mf) : _ ->
                    let f' = fromJust mf
                     in if f' >= f && not (isNaN f') && not (isInfinite f')
                            then canonical ec' >>= \ec -> tryImprove env ec f' f n go
                            else go (n + 1) (max f f')

egraphGP ::
    RegressionConfig ->
    String -> -- nonterminals
    String -> -- varnames
    [(DataSet, DataSet)] ->
    [DataSet] ->
    StateT EGraph (StateT StdGen IO) [Fix SRTree]
egraphGP cfg nonterminals varnames dataTrainVals dataTests =
    let env = mkEGPEnv cfg nonterminals varnames dataTrainVals dataTests
     in do
            loadState env
            _ <- insertTerms env
            evaluateUnevaluated (envFitFun env)
            t0 <- io getPOSIXTime
            pop <- initPopulation env >>= Prelude.mapM canonical
            out <- tracePopulation env (Prelude.zip [0 ..] pop)
            _ <-
                iterateFor
                    (generations cfg)
                    t0
                    (maxTimeMaybe env)
                    (pop, out, populationSize cfg)
                    (runGeneration env)
            saveState env >> paretoFront' env (maxExpressionSize cfg)
