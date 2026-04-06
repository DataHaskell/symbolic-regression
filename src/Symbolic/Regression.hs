{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Internal.Expression
import System.Random
import Data.IORef

import Control.Monad (
    foldM,
    forM,
    forM_,
    replicateM,
    when,
    (>=>),
 )
import Data.Function (on)
import qualified Data.IntMap.Strict as IM
import Data.List (
    intercalate,
    maximumBy,
    sortBy,
    zip4,
 )
import Data.Ord (Down(..), comparing)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (Refl))
import Type.Reflection (typeRep)

import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX

-- Internal modules (hegg-based)
import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Utils
import Symbolic.Regression.Expr.Eval (evalTree)
import Symbolic.Regression.Expr.Opt (Distribution(..), nll, r2, fractionalBayesFactor, minimizeNLL)
import Symbolic.Regression.Expr.Print (showExpr, showExprWithVars, showPython, showLatex, showLatexWithVars)
import Symbolic.Regression.Language ()  -- instances only
import Symbolic.Regression.Rewrites ()  -- instances only
import Symbolic.Regression.EGraph

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
    , numIslands :: Int
    -- ^ Number of independent sub-populations (island model). (default: 15)
    , migrationFraction :: Double
    -- ^ Fraction of each island replaced by migrants each generation. (default: 0.05)
    }

data ValidationConfig = ValidationConfig
    { validationPercent :: Double
    , validationSeed :: Int
    }

------------------------------------------------------------------------
-- Dataset type (replacing srtree's 3-tuple with our 2-tuple + optional error)
------------------------------------------------------------------------

-- | A dataset: feature matrix, target vector, and optional error vector.
type OldDataSet = (Features, PVector, Maybe PVector)

-- | Extract the feature matrix from a dataset.
getX :: OldDataSet -> Features
getX (x, _, _) = x

-- | Extract the target vector from a dataset.
getY :: OldDataSet -> PVector
getY (_, y, _) = y

-- | Extract the optional error vector from a dataset.
getYErr :: OldDataSet -> Maybe PVector
getYErr (_, _, e) = e

------------------------------------------------------------------------
-- Monad and type aliases
------------------------------------------------------------------------

-- | The RndEGraph monad: e-graph state over random generator over IO.
type RndEGraph a = StateT SREGraph (StateT StdGen IO) a

-- | Type alias for the multi-view fitness function used throughout the algorithm.
type FitFun = Fix ExprF -> RndEGraph (Double, [PVector])

------------------------------------------------------------------------
-- Small random utility functions (internalized from srtree)
------------------------------------------------------------------------

-- | Pick a random element from a non-empty list.
randomFrom :: [a] -> StateT StdGen IO a
randomFrom xs = do
    g <- get
    let (i, g') = randomR (0, length xs - 1) g
    put g'
    pure (xs !! i)

-- | Random integer in a range.
randomRange :: (Random a) => (a, a) -> StateT StdGen IO a
randomRange range_ = do
    g <- get
    let (v, g') = randomR range_ g
    put g'
    pure v

-- | Fair coin toss.
toss :: StateT StdGen IO Bool
toss = randomRange (False, True)

-- | Biased coin toss.
tossBiased :: Double -> StateT StdGen IO Bool
tossBiased p = do
    r <- randomRange (0.0 :: Double, 1.0)
    pure (r < p)

-- | Lift IO into the RndEGraph monad.
io :: IO a -> RndEGraph a
io = liftIO

-- | Lift a random action (StateT StdGen IO) into the RndEGraph monad.
rnd :: StateT StdGen IO a -> RndEGraph a
rnd = lift

------------------------------------------------------------------------
-- Parsing non-terminals from config strings
------------------------------------------------------------------------

-- | Parse a comma-separated string of operator names into ExprF templates.
parseNonTerms :: String -> [ExprF ()]
parseNonTerms s =
    [ parseOne tok | tok <- splitOn "," s, not (null tok) ]
  where
    parseOne "add"    = BinF Add () ()
    parseOne "sub"    = BinF Sub () ()
    parseOne "mul"    = BinF Mul () ()
    parseOne "div"    = BinF Div () ()
    parseOne "pow"    = BinF Pow () ()
    parseOne "square" = UnF Sq ()
    parseOne "cube"   = UnF Cube ()
    parseOne "log"    = UnF Log ()
    parseOne "recip"  = UnF Recip ()
    parseOne "sqrt"   = UnF Sqrt ()
    parseOne "exp"    = UnF Exp ()
    parseOne "sin"    = UnF Sin ()
    parseOne "cos"    = UnF Cos ()
    parseOne "abs"    = UnF Abs ()
    parseOne "neg"    = UnF Neg ()
    parseOne name     = error ("parseNonTerms: unknown operator: " ++ name)

------------------------------------------------------------------------
-- Shared environment
------------------------------------------------------------------------

-- | Cache of extracted best expressions, keyed by canonical ClassId.
-- Invalidated after saturation or graph compaction.
type ExprCache = IORef (IM.IntMap (Fix ExprF))

-- | Shared environment threaded through all egraphGP helpers.
data EGPEnv = EGPEnv
    { envCfg :: RegressionConfig
    , envFitFun :: FitFun
    , envTerms :: [Fix ExprF]
    , envParams :: [Fix ExprF]
    , envNonTerms :: [ExprF ()]
    , envUniNonTerms :: [ExprF ()]
    , envBinNonTerms :: [ExprF ()]
    , envShouldReparam :: Bool
    , envRelabel :: Fix ExprF -> Fix ExprF
    , envVarnames :: String
    , envDataTrainVals :: [(OldDataSet, OldDataSet)]
    , envDataTests :: [OldDataSet]
    , envExprCache :: !ExprCache  -- ^ Extraction cache
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
        , maxExpressionSize = 15
        , validationConfig = Nothing
        , showTrace = True
        , lossFunction = MSE
        , numOptimisationIterations = 30
        , numParameterRetries = 2
        , populationSize = 100
        , tournamentSize = 10
        , crossoverProbability = 0.95
        , mutationProbability = 0.3
        , numParams = -1
        , generational = False
        , simplifyExpressions = True
        , maxTime = -1
        , unaryFunctions = [(`F.pow` 2), (`F.pow` 3), log, (1 /), sqrt, exp, sin, cos]
        , binaryFunctions = [(+), (-), (*), (/)]
        , dumpTo = ""
        , loadFrom = ""
        , numIslands = 15
        , migrationFraction = 0.05
        }

-- Maximum e-node count before compaction is triggered.
maxMem :: Int
maxMem = 2000000

-- | True for unary ExprF nodes.
isUni :: ExprF a -> Bool
isUni (UnF _ _) = True
isUni _ = False

-- | True for binary ExprF nodes.
isBin :: ExprF a -> Bool
isBin (BinF{}) = True
isBin _ = False

------------------------------------------------------------------------
-- DataFrame integration
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Main API
------------------------------------------------------------------------

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
        fm d = toFeatureMatrix targetColumn d
        tgt d = VU.fromList (D.columnAsList targetColumn d)
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
                emptySREGraph
     in fmap (Prelude.map (toExpr cols)) (evalStateT alg g)

-- | Convert a unary ExprF operation to the equivalent DataFrame expression.
toExprUni :: [T.Text] -> UnOp -> Fix ExprF -> Expr Double
toExprUni cols Sq v    = F.pow (toExpr cols v) 2
toExprUni cols Cube v  = F.pow (toExpr cols v) 3
toExprUni cols Log v   = log (toExpr cols v)
toExprUni cols Recip v = F.lit 1 / toExpr cols v
toExprUni cols Sqrt v  = sqrt (toExpr cols v)
toExprUni cols Exp v   = exp (toExpr cols v)
toExprUni cols Sin v   = sin (toExpr cols v)
toExprUni cols Cos v   = cos (toExpr cols v)
toExprUni cols Abs v   = abs (toExpr cols v)
toExprUni cols Neg v   = negate (toExpr cols v)

-- | Convert a binary ExprF operation to the equivalent DataFrame expression.
toExprBin :: [T.Text] -> BinOp -> Fix ExprF -> Fix ExprF -> Expr Double
toExprBin cols Add l r = toExpr cols l + toExpr cols r
toExprBin cols Sub l r = toExpr cols l - toExpr cols r
toExprBin cols Mul l r = toExpr cols l * toExpr cols r
toExprBin cols Div l r = toExpr cols l / toExpr cols r
toExprBin cols Pow l r = toExpr cols l ** toExpr cols r

-- | Recursively convert a fixed-point ExprF to a DataFrame expression.
toExpr :: [T.Text] -> Fix ExprF -> Expr Double
toExpr _    (Fix (LitF v))      = Lit v
toExpr cols (Fix (VarF ix))     = Col (cols !! ix)
toExpr _    (Fix (ParamF _))    = Lit 0  -- params should be substituted before calling
toExpr cols (Fix (UnF f v))     = toExprUni cols f v
toExpr cols (Fix (BinF op l r)) = toExprBin cols op l r

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
        | name == "sqrt" = "sqrt"
        | name == "exp" = "exp"
        | name == "sin" = "sin"
        | name == "cos" = "cos"
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

------------------------------------------------------------------------
-- Fitness evaluation with multi-view support
------------------------------------------------------------------------

-- | Evaluate fitness across multiple train/validation splits.
fitnessMV ::
    Bool ->
    Int ->
    Int ->
    Distribution ->
    [(OldDataSet, OldDataSet)] ->
    Fix ExprF ->
    RndEGraph (Double, [PVector])
fitnessMV shouldReparam nRetries nIter dist dataTrainVals tree' = do
    let tree = if shouldReparam then relabelParams tree' else relabelParamsOrder tree'
        nParams' = countParamsUniq tree
    if nParams' == 0
        then do
            let results = flip Prelude.map dataTrainVals $ \(dtrain, _dval) ->
                    let yHat = evalTree (getX dtrain) VU.empty tree
                        loss = computeLoss dist yHat (getY dtrain)
                    in (loss, VU.empty)
                avgLoss = sum (Prelude.map fst results) / fromIntegral (length results)
                fit' = if isNaN avgLoss || isInfinite avgLoss then -1e18 else negate avgLoss
            pure (fit', Prelude.map snd results)
        else do
            results <- forM dataTrainVals $ \(dtrain, _dval) -> do
                let theta0s = replicate nRetries (VU.replicate nParams' 1.0)
                bestResult <- foldM (\(!bestF, !bestTh) th0 -> do
                    g <- rnd get
                    let (!rndTh, !g') = randomTheta nParams' g
                    rnd (put g')
                    let th = if VU.null th0 then rndTh else th0
                        (!thetaOpt, !loss, _) = minimizeNLL dist (getYErr dtrain) nIter
                            (getX dtrain) (getY dtrain) tree th
                        !fit' = if isNaN loss || isInfinite loss then -1e18 else negate loss
                    pure $ if fit' > bestF then (fit', thetaOpt) else (bestF, bestTh)
                    ) (-1e18, VU.empty) theta0s
                pure bestResult
            let avgFit = sum (Prelude.map fst results) / fromIntegral (length results)
            pure (avgFit, Prelude.map snd results)
  where
    randomTheta :: Int -> StdGen -> (PVector, StdGen)
    randomTheta n g = go n g []
      where
        go :: Int -> StdGen -> [Double] -> (PVector, StdGen)
        go 0 g' acc = (VU.fromList (reverse acc), g')
        go k g' acc =
            let (v, g'') = randomR (-1.0 :: Double, 1.0) g'
            in go (k-1) g'' (v : acc)

-- | Simple MSE loss computation.
computeLoss :: Distribution -> PVector -> PVector -> Double
computeLoss MSE yHat yTrue =
    let n = VU.length yTrue
        s = VU.sum (VU.zipWith (\a b -> (a - b) ** 2) yTrue yHat)
    in s / fromIntegral n
computeLoss _ yHat yTrue = computeLoss MSE yHat yTrue

------------------------------------------------------------------------
-- Metrics
------------------------------------------------------------------------

-- | Compute NLL, R2, and fractional Bayes factor across train, validation, and test sets.
computeDatasetMetrics ::
    Fix ExprF ->
    Distribution ->
    Double ->
    PVector ->
    OldDataSet ->
    OldDataSet ->
    OldDataSet ->
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

------------------------------------------------------------------------
-- Environment construction
------------------------------------------------------------------------

-- | Compute terminal and parameter lists from config and training data.
mkEnvTerminals ::
    RegressionConfig -> [(OldDataSet, OldDataSet)] -> ([Fix ExprF], [Fix ExprF])
mkEnvTerminals cfg dataTrainVals =
    let nFeats = case dataTrainVals of
            [] -> 0
            (h : _) ->
                let xss = getX (fst h)
                in if V.null xss then 0 else VU.length (xss V.! 0)
        ps =
            if numParams cfg == -1
                then [param 0]
                else Prelude.map param [0 .. numParams cfg - 1]
        ts = [var ix | ix <- [0 .. nFeats - 1]]
     in (ts, ps)

-- | Build the shared environment for egraphGP helpers.
mkEGPEnv ::
    RegressionConfig ->
    String ->
    String ->
    [(OldDataSet, OldDataSet)] ->
    [OldDataSet] ->
    ExprCache ->
    EGPEnv
mkEGPEnv cfg nonterminals varnames dataTrainVals dataTests cache =
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
            , envExprCache = cache
            }

------------------------------------------------------------------------
-- E-graph operations (adapted to hegg via EGraph.hs)
------------------------------------------------------------------------

-- | Insert a tree into the e-graph, returning its canonical class ID.
insertTreeM :: Fix ExprF -> RndEGraph ClassId
insertTreeM tree = do
    eg <- get
    let (!cid, !eg') = insertTree tree eg
    put eg'
    pure cid

-- | Get the canonical class ID in the RndEGraph monad.
canonicalM :: ClassId -> RndEGraph ClassId
canonicalM cid = gets (canonical cid)

-- | Get the best expression for an e-class (uncached — prefer cachedExtract).
getBestExprM :: ClassId -> RndEGraph (Fix ExprF)
getBestExprM cid = gets (getBestExpr cid)

-- | Get the best expression with caching. Avoids redundant extractBest DP.
cachedExtract :: ExprCache -> ClassId -> RndEGraph (Fix ExprF)
cachedExtract cacheRef cid' = do
    cid <- canonicalM cid'
    cache <- liftIO (readIORef cacheRef)
    case IM.lookup cid cache of
        Just e  -> pure e
        Nothing -> do
            e <- getBestExprM cid
            liftIO $ modifyIORef' cacheRef (IM.insert cid e)
            pure e

-- | Clear the extraction cache (call after saturation or graph compaction).
clearExprCache :: ExprCache -> RndEGraph ()
clearExprCache cacheRef = liftIO $ writeIORef cacheRef IM.empty

-- | Get the fitness of an e-class.
getFitnessM :: ClassId -> RndEGraph (Maybe Double)
getFitnessM cid = gets (getFitness cid)

-- | Get the size of an e-class.
getSizeM :: ClassId -> RndEGraph Int
getSizeM cid = gets (getSize cid)

-- | Insert fitness and theta into an e-class.
insertFitnessM :: ClassId -> Double -> [PVector] -> RndEGraph ()
insertFitnessM cid f theta = modify' (insertFitness cid f theta)

-- | Get the top-k e-classes with a given size.
getTopFitEClassWithSize :: Int -> Int -> RndEGraph [(ClassId, Double)]
getTopFitEClassWithSize sz k = gets (getTopFitWithSize sz k)

-- | Get the top-k e-classes that satisfy a predicate.
getTopFitEClassThat :: Int -> (ClassId -> Bool) -> RndEGraph [(ClassId, Double)]
getTopFitEClassThat k _predicate = do
    results <- gets (getTopFit (k * 10))  -- get more and filter
    pure $ take k results

-- | Get all class IDs.
getAllClassIdsM :: RndEGraph [ClassId]
getAllClassIdsM = gets getAllClassIds

-- | Evaluate fitness if not yet computed.
updateIfNothing :: ExprCache -> FitFun -> ClassId -> RndEGraph ()
updateIfNothing cache fitFun cid' = do
    cid <- canonicalM cid'
    mf <- getFitnessM cid
    case mf of
        Just _ -> pure ()
        Nothing -> do
            tree <- cachedExtract cache cid
            (f, theta) <- fitFun tree
            insertFitnessM cid f theta

-- | Evaluate fitness for all unevaluated e-classes.
evaluateUnevaluated :: ExprCache -> FitFun -> RndEGraph ()
evaluateUnevaluated cache fitFun = do
    eg <- get
    let unevaluated = filter (\c -> isNothing (getFitness c eg)) (getAllClassIds eg)
    forM_ unevaluated $ \cid -> do
        tree <- cachedExtract cache cid
        (f, theta) <- fitFun tree
        insertFitnessM cid f theta

-- | Run equality saturation on the current e-graph.
-- Clears the extraction cache since merges invalidate class identities.
runSaturationM :: ExprCache -> RndEGraph ()
runSaturationM cache = do
    modify' runSaturation
    clearExprCache cache

-- | Insert a random expression into the e-graph.
insertRndExpr :: Int -> StateT StdGen IO (Fix ExprF) -> StateT StdGen IO (ExprF ()) -> RndEGraph ClassId
insertRndExpr maxSz rndT rndNT = do
    tree <- rnd $ buildRandomTree maxSz rndT rndNT
    insertTreeM tree

-- | Build a random expression tree.
buildRandomTree :: Int -> StateT StdGen IO (Fix ExprF) -> StateT StdGen IO (ExprF ()) -> StateT StdGen IO (Fix ExprF)
buildRandomTree maxSz rndT rndNT
    | maxSz <= 1 = rndT
    | otherwise = do
        coin <- toss
        if coin
            then rndT
            else do
                nt <- rndNT
                case nt of
                    BinF op _ _ -> do
                        let half = max 1 (maxSz `div` 2)
                        l <- buildRandomTree half rndT rndNT
                        r <- buildRandomTree half rndT rndNT
                        pure $ Fix (BinF op l r)
                    UnF f _ -> do
                        c <- buildRandomTree (maxSz - 1) rndT rndNT
                        pure $ Fix (UnF f c)
                    _ -> rndT

-- | Simplify an expression using equality saturation in a temporary e-graph.
simplifyEqSatDefault :: Fix ExprF -> Fix ExprF
simplifyEqSatDefault expr =
    let eg0 = emptySREGraph
        (!cid, !eg1) = insertTree expr eg0
        !eg2 = runSaturation eg1
    in getBestExpr cid eg2

-- | Get the best expression for each size tier.
getBestExprWithSize :: Int -> RndEGraph [(ClassId, Maybe Double)]
getBestExprWithSize sz = do
    results <- getTopFitEClassWithSize sz 1
    pure [(cid, Just f) | (cid, f) <- results]

------------------------------------------------------------------------
-- Population management
------------------------------------------------------------------------

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

-- | Load state -- no-op (hegg EGraph lacks Binary instances).
loadState :: EGPEnv -> RndEGraph ()
loadState _env = pure ()

-- | Save state -- no-op (hegg EGraph lacks Binary instances).
saveState :: EGPEnv -> RndEGraph ()
saveState _env = pure ()

-- | Pre-populate the e-graph with all terminal symbols (variables and params).
insertTerms :: EGPEnv -> RndEGraph [ClassId]
insertTerms env = forM (envTerms env) (insertTreeM >=> canonicalM)

-- | Sample a random terminal: variable, constant, or learnable parameter.
rndTerm :: EGPEnv -> StateT StdGen IO (Fix ExprF)
rndTerm env = do
    coin <- toss
    if coin || numParams (envCfg env) == 0
        then randomFrom (envTerms env)
        else randomFrom (envParams env)

-- | Sample a random non-terminal (operation node).
rndNonTerm :: EGPEnv -> StateT StdGen IO (ExprF ())
rndNonTerm env = randomFrom (envNonTerms env)

-- | Re-evaluate fitness for classes that may have changed.
-- With hegg, rebuild handles structural invariants; we just re-fit all classes
-- that don't have fitness yet.
refitChanged :: EGPEnv -> RndEGraph ()
refitChanged env = do
    ids <- getAllClassIdsM
    forM_ ids $ \cid -> do
        mf <- getFitnessM cid
        case mf of
            Just _ -> pure ()
            Nothing -> do
                tree <- cachedExtract (envExprCache env) cid
                (f, theta) <- envFitFun env tree
                insertFitnessM cid f theta

-- | Generate the initial random population and evaluate fitness for each member.
initPopulation :: EGPEnv -> RndEGraph [ClassId]
initPopulation env = replicateM (populationSize (envCfg env)) $ do
    ec <-
        insertRndExpr (maxExpressionSize (envCfg env)) (rndTerm env) (rndNonTerm env)
            >>= canonicalM
    _ <- updateIfNothing (envExprCache env) (envFitFun env) ec
    pure ec

-- | Emit formatted expression rows for all indexed members when tracing is on.
tracePopulation :: EGPEnv -> [(Int, ClassId)] -> RndEGraph [[String]]
tracePopulation env indexed =
    if showTrace (envCfg env)
        then forM indexed (uncurry (printExpr' env))
        else pure []

-- | Combine Pareto-front elites with remaining offspring to form next population.
selectNewPop :: EGPEnv -> Bool -> [ClassId] -> RndEGraph [ClassId]
selectNewPop env full newPop' = do
    pareto <-
        concat
            <$> forM [1 .. maxExpressionSize (envCfg env)] (`getTopFitEClassWithSize` 2)
    let paretoIds = Prelude.map fst pareto
        remainder = populationSize (envCfg env) - length paretoIds
    lft <-
        if full
            then Prelude.map fst <$> getTopFitEClassThat remainder (const True)
            else pure $ Prelude.take remainder newPop'
    Prelude.mapM canonicalM (paretoIds <> lft)

-- | Compact e-graph by discarding all nodes except the Pareto-front expressions.
cleanEGraph :: EGPEnv -> RndEGraph ()
cleanEGraph env = do
    io . putStrLn $ "cleaning"
    pareto <-
        forM [1 .. maxExpressionSize (envCfg env)] (`getTopFitEClassWithSize` 10)
            >>= Prelude.mapM canonicalM . Prelude.map fst . concat
    fitnesses <- forM pareto $ \c -> do
        mf <- getFitnessM c
        mth <- gets (getTheta c)
        pure (mf, mth)
    exprs <- forM pareto (cachedExtract (envExprCache env))
    put emptySREGraph
    clearExprCache (envExprCache env)
    newIds <- forM (Prelude.map (envRelabel env) exprs) insertTreeM
    let restore (eId, (Just f, Just th)) = insertFitnessM eId f th
        restore (_, _) = pure ()
    forM_ (Prelude.zip newIds (Prelude.reverse fitnesses)) restore

------------------------------------------------------------------------
-- Tournament selection
------------------------------------------------------------------------

-- | Run one tournament over challengers; return the fittest.
applyTournament :: EGPEnv -> [ClassId] -> RndEGraph ClassId
applyTournament env xs = do
    challengers <-
        replicateM (tournamentSize (envCfg env)) (rnd $ randomFrom xs)
            >>= traverse canonicalM
    fits <- Prelude.mapM getFitnessM challengers
    let rated = [(f, c) | (Just f, c) <- Prelude.zip fits challengers]
    if null rated
        then rnd $ randomFrom xs
        else pure . snd . maximumBy (compare `on` fst) $ rated

-- | Select two parents by running independent tournaments.
tournament :: EGPEnv -> [ClassId] -> RndEGraph (ClassId, ClassId)
tournament env xs = do
    p1 <- applyTournament env xs >>= canonicalM
    p2 <- applyTournament env xs >>= canonicalM
    pure (p1, p2)

------------------------------------------------------------------------
-- Mutation operators
------------------------------------------------------------------------

-- | Apply a weighted random mutation operator to a parent pair.
-- Weights mirror PySR's mutation distribution for structural diversity.
combine :: EGPEnv -> (ClassId, ClassId) -> RndEGraph ClassId
combine env (p1, p2) = do
    r <- rnd $ randomRange (0 :: Int, 99)
    let maxSz = maxExpressionSize (envCfg env)
    result <- if
          | r < 36 -> mutateOperator env p1
          | r < 60 -> insertNode env p1 maxSz
          | r < 74 -> rotateTree env p1
          | r < 84 -> crossover env p1 p2
          | r < 88 -> deleteNode env p1
          | r < 92 -> pure p1
          | r < 95 -> mutate env p1
          | otherwise -> insertRndExpr maxSz (rndTerm env) (rndNonTerm env)
    canonicalM result

-- | Safely get the best expression for an e-class, or Nothing if it was cleaned.
safeBestExpr :: EGPEnv -> ClassId -> RndEGraph (Maybe (Fix ExprF))
safeBestExpr env p = do
    p' <- canonicalM p
    ids <- getAllClassIdsM
    if p' `elem` ids
        then Just <$> cachedExtract (envExprCache env) p'
        else pure Nothing

-- | Apply a function at a random position in the tree (preorder indexing).
modifyAtPos :: Int -> (Fix ExprF -> Fix ExprF) -> Fix ExprF -> Fix ExprF
modifyAtPos 0 f t = f t
modifyAtPos n f (Fix (BinF op l r)) =
    let lSz = countNodes l
    in if n - 1 < lSz
       then Fix (BinF op (modifyAtPos (n-1) f l) r)
       else Fix (BinF op l (modifyAtPos (n-1-lSz) f r))
modifyAtPos n f (Fix (UnF g c)) = Fix (UnF g (modifyAtPos (n-1) f c))
modifyAtPos _ _ t = t

-- | Apply a tree mutation at a random position, safely handling cleaned classes.
deepMutate :: EGPEnv -> ClassId -> (Fix ExprF -> Fix ExprF) -> RndEGraph ClassId
deepMutate env p f = do
    mt <- safeBestExpr env p
    case mt of
        Nothing -> pure p
        Just tree -> do
            let sz = countNodes tree
            pos <- rnd $ randomRange (0, max 0 (sz - 1))
            let tree' = modifyAtPos pos f tree
            insertTreeM (envRelabel env tree') >>= canonicalM

-- | Replace a random operator with another of the same arity (at random position).
mutateOperator :: EGPEnv -> ClassId -> RndEGraph ClassId
mutateOperator env p = do
    newUni <- rnd $ randomFrom (envUniNonTerms env)
    newBin <- rnd $ randomFrom (envBinNonTerms env)
    let swapOp (Fix (UnF _ c)) = case newUni of { UnF f' _ -> Fix (UnF f' c); _ -> Fix (UnF Sq c) }
        swapOp (Fix (BinF _ l r)) = case newBin of { BinF op' _ _ -> Fix (BinF op' l r); _ -> Fix (BinF Add l r) }
        swapOp t = t
    deepMutate env p swapOp

-- | Insert a new unary operator wrapping a random subtree.
insertNode :: EGPEnv -> ClassId -> Int -> RndEGraph ClassId
insertNode env p maxSz = do
    sz <- getSizeM p
    if sz >= maxSz then pure p
    else do
        newUni <- rnd $ randomFrom (envUniNonTerms env)
        let f = case newUni of { UnF f' _ -> f'; _ -> Sq }
            wrap t = Fix (UnF f t)
        deepMutate env p wrap

-- | Swap children of a random binary node.
rotateTree :: EGPEnv -> ClassId -> RndEGraph ClassId
rotateTree env p =
    let swapKids (Fix (BinF op l r)) = Fix (BinF op r l)
        swapKids t = t
    in deepMutate env p swapKids

-- | Remove a random operator (collapse unary, keep left of binary).
deleteNode :: EGPEnv -> ClassId -> RndEGraph ClassId
deleteNode env p =
    let collapse (Fix (UnF _ c)) = c
        collapse (Fix (BinF _ l _)) = l
        collapse t = t
    in deepMutate env p collapse

------------------------------------------------------------------------
-- Crossover (simplified tree-based approach)
------------------------------------------------------------------------

-- | Subtree crossover: swap a random subtree of p1 with a subtree from p2.
crossover :: EGPEnv -> ClassId -> ClassId -> RndEGraph ClassId
crossover env p1 p2 = do
    sz <- getSizeM p1
    coin <- rnd $ tossBiased (crossoverProbability (envCfg env))
    if sz <= 1 || not coin
        then do
            choice <- rnd $ randomFrom [p1, p2]
            pure choice
        else do
            tree1 <- cachedExtract (envExprCache env) p1
            tree2 <- cachedExtract (envExprCache env) p2
            let sz1 = countNodes tree1
                sz2 = countNodes tree2
            pos <- rnd $ randomRange (1, max 1 (sz1 - 1))
            donorPos <- rnd $ randomRange (0, max 0 (sz2 - 1))
            let donor = getSubAt donorPos tree2
                newTree = replaceSubAt pos donor tree1
            insertTreeM (envRelabel env newTree) >>= canonicalM

-- | Get the subtree at a given position (preorder).
getSubAt :: Int -> Fix ExprF -> Fix ExprF
getSubAt 0 t = t
getSubAt n (Fix node) = case node of
    BinF _ l r ->
        let lSz = countNodes l
        in if n - 1 < lSz then getSubAt (n-1) l else getSubAt (n-1-lSz) r
    UnF _ c -> getSubAt (n-1) c
    _ -> Fix node

-- | Replace the subtree at a given position (preorder).
replaceSubAt :: Int -> Fix ExprF -> Fix ExprF -> Fix ExprF
replaceSubAt 0 replacement _ = replacement
replaceSubAt n replacement (Fix node) = case node of
    BinF op l r ->
        let lSz = countNodes l
        in if n - 1 < lSz
           then Fix (BinF op (replaceSubAt (n-1) replacement l) r)
           else Fix (BinF op l (replaceSubAt (n-1-lSz) replacement r))
    UnF f c -> Fix (UnF f (replaceSubAt (n-1) replacement c))
    _ -> Fix node

------------------------------------------------------------------------
-- Mutation (simplified tree-based approach)
------------------------------------------------------------------------

-- | Randomly replace one subtree; no-op with probability (1 - mutationProbability).
mutate :: EGPEnv -> ClassId -> RndEGraph ClassId
mutate env p = do
    coin <- rnd $ tossBiased (mutationProbability (envCfg env))
    if coin
        then do
            tree <- cachedExtract (envExprCache env) p
            let sz = countNodes tree
            pos <- rnd $ randomRange (0, sz - 1)
            let maxSz = max 1 (maxExpressionSize (envCfg env) - sz + 1)
            newSub <- rnd $ buildRandomTree maxSz (rndTerm env) (rndNonTerm env)
            let newTree = replaceSubAt pos newSub tree
            insertTreeM (envRelabel env newTree) >>= canonicalM
        else pure p

------------------------------------------------------------------------
-- Evolution loop
------------------------------------------------------------------------

-- | Produce one offspring via tournament selection, crossover, mutation, and e-graph rewriting.
evolve :: EGPEnv -> [ClassId] -> RndEGraph ClassId
evolve env xs' = do
    xs <- Prelude.mapM canonicalM xs'
    parents' <- tournament env xs
    offspring <- combine env parents'
    when (simplifyExpressions (envCfg env)) $ do
        nClasses <- gets eGraphClassCount
        when (nClasses < 1500) (runSaturationM (envExprCache env))
        refitChanged env
    canonicalM offspring >>= \oc -> updateIfNothing (envExprCache env) (envFitFun env) oc >> pure ()
    canonicalM offspring

------------------------------------------------------------------------
-- Pareto front extraction and output formatting
------------------------------------------------------------------------

-- | Best expression, optionally simplified, with params relabeled.
getBestRelabeled :: EGPEnv -> ClassId -> RndEGraph (Fix ExprF)
getBestRelabeled env ec = do
    bestExpr <-
        (if simplifyExpressions (envCfg env) then simplifyEqSatDefault else id)
            <$> cachedExtract (envExprCache env) ec
    pure $
        if envShouldReparam env
            then relabelParams bestExpr
            else relabelParamsOrder bestExpr

-- | Re-optimize parameters when their count has changed since last evaluation.
refitIfNeeded :: EGPEnv -> Fix ExprF -> Maybe [PVector] -> RndEGraph [PVector]
refitIfNeeded env best' thetas' = do
    let nParams' = countParamsUniq best'
        nThetas = fmap (Prelude.map VU.length) thetas'
    if maybe False (Prelude.any (/= nParams')) nThetas
        then snd <$> envFitFun env best'
        else pure (fromMaybe [] thetas')

-- | Format one CSV row: index, view, expression (text/Python/LaTeX), params, metrics.
formatRow ::
    EGPEnv -> Int -> Int -> Fix ExprF -> Fix ExprF -> String -> String -> String
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
            <> show @Int (countNodes expr)
            <> ","
            <> vals

-- | Emit formatted output rows for an e-class across all data views.
printExpr' :: EGPEnv -> Int -> ClassId -> RndEGraph [String]
printExpr' env ix ec' = do
    ec <- canonicalM ec'
    thetas' <- gets (getTheta ec)
    best' <- getBestRelabeled env ec
    thetas <- refitIfNeeded env best' thetas'
    maxLoss <- negate . fromJust <$> getFitnessM ec
    let dist = lossFunction (envCfg env)
    forM
        (Data.List.zip4 [(0 :: Int) ..] (envDataTrainVals env) (envDataTests env) thetas) $
        \(view, (dataTrain, dataVal), dataTest, theta') ->
            let expr = paramsToConst (VU.toList theta') best'
                thetaStr = intercalate ";" $ Prelude.map show (VU.toList theta')
                vals =
                    computeDatasetMetrics best' dist maxLoss theta' dataTrain dataVal dataTest
             in pure $ formatRow env ix view expr best' thetaStr vals

-- | Best expression, optionally simplified, with params relabeled (used for Pareto front).
extractBestExpr :: EGPEnv -> ClassId -> RndEGraph (Fix ExprF)
extractBestExpr env ec =
    relabelParams
        . (if simplifyExpressions (envCfg env) then simplifyEqSatDefault else id)
        <$> cachedExtract (envExprCache env) ec

-- | Record this e-class on the Pareto front and recurse to the next complexity tier.
tryImprove ::
    EGPEnv ->
    ClassId ->
    Double ->
    Double ->
    Int ->
    (Int -> Double -> RndEGraph [Fix ExprF]) ->
    RndEGraph [Fix ExprF]
tryImprove env ec _f' _f n goFn = do
    thetas' <- gets (getTheta ec)
    bestExpr <- extractBestExpr env ec
    let t = case thetas' of
            Just (h : _) -> paramsToConst (VU.toList h) bestExpr
            _ -> Fix (LitF 0)
    ts <- goFn (n + 1) (max _f _f')
    pure (t : ts)

-- | Extract Pareto front: best expression per complexity level with strictly improving fitness.
paretoFront' :: EGPEnv -> Int -> RndEGraph [Fix ExprF]
paretoFront' env maxSize' = go 1 (-(1.0 / 0.0))
  where
    go :: Int -> Double -> RndEGraph [Fix ExprF]
    go n f
        | n > maxSize' = pure []
        | otherwise = do
            ecList <- getBestExprWithSize n
            case ecList of
                [] -> go (n + 1) f
                (ec', mf) : _ ->
                    let f' = fromJust mf
                     in if f' >= f && not (isNaN f') && not (isInfinite f')
                            then canonicalM ec' >>= \ec -> tryImprove env ec f' f n go
                            else go (n + 1) (max f f')

------------------------------------------------------------------------
-- Island model
------------------------------------------------------------------------

-- | Migrate best individuals between islands. Replace migrationFraction of each
-- island with the best individuals from other islands.
migrateIslands :: EGPEnv -> [[ClassId]] -> RndEGraph [[ClassId]]
migrateIslands env islands = do
    let nMigrate = max 1 $ round (migrationFraction (envCfg env) * fromIntegral (populationSize (envCfg env)))
    -- Collect best from each island
    bests <- forM islands $ \pop -> do
        fits <- forM pop $ \ec -> do
            mf <- getFitnessM ec
            pure (fromMaybe (-1e18) mf, ec)
        pure . Prelude.map snd . Prelude.take nMigrate . sortBy (comparing (Down . fst)) $ fits
    -- For each island, replace worst with best from other islands
    forM (Prelude.zip [(0 :: Int)..] islands) $ \(i, pop) -> do
        let migrants = concat [b | (j, b) <- Prelude.zip [(0 :: Int)..] bests, j /= i]
            nReplace = min nMigrate (length migrants)
        if nReplace == 0 || null migrants
            then pure pop
            else do
                selected <- replicateM nReplace (rnd $ randomFrom migrants) >>= Prelude.mapM canonicalM
                pure $ selected ++ Prelude.drop nReplace pop

------------------------------------------------------------------------
-- Main GP loop
------------------------------------------------------------------------

egraphGP ::
    RegressionConfig ->
    String -> -- nonterminals
    String -> -- varnames
    [(OldDataSet, OldDataSet)] ->
    [OldDataSet] ->
    StateT SREGraph (StateT StdGen IO) [Fix ExprF]
egraphGP cfg nonterminals varnames dataTrainVals dataTests = do
    cache <- io (newIORef IM.empty)
    let env = mkEGPEnv cfg nonterminals varnames dataTrainVals dataTests cache
        nIslands = numIslands cfg
        islandSize = max 10 (populationSize cfg `div` nIslands)
        islandCfg = cfg { populationSize = islandSize }
        islandEnv = env { envCfg = islandCfg }
     in do
            loadState env
            _ <- insertTerms env
            evaluateUnevaluated (envExprCache env) (envFitFun env)
            t0 <- io getPOSIXTime
            -- Initialize islands
            islands <- replicateM nIslands (initPopulation islandEnv >>= Prelude.mapM canonicalM)
            _out <- tracePopulation env (Prelude.zip [0 ..] (concat islands))
            -- Run generations with migration
            let runIslandGen _ (pops, o, ix) = do
                    -- Evolve each island independently
                    pops' <- forM pops $ \pop -> do
                        newPop <- replicateM islandSize (evolve islandEnv pop)
                        -- Compact if needed
                        totSz <- gets eGraphNodeCount
                        let full = totSz > max maxMem islandSize
                        when full (cleanEGraph islandEnv)
                        selectNewPop islandEnv full newPop
                    -- Migrate between islands
                    pops'' <- migrateIslands islandEnv pops'
                    o' <- tracePopulation env (Prelude.zip [ix ..] (concat pops''))
                    pure (pops'', o <> o', ix + islandSize * nIslands)
            (finalIslands, _, _) <-
                iterateFor
                    (generations cfg)
                    t0
                    (maxTimeMaybe env)
                    (islands, _out, islandSize * nIslands)
                    runIslandGen
            -- Merge all island results for Pareto extraction
            _ <- Prelude.mapM canonicalM (concat finalIslands)
            saveState env >> paretoFront' env (maxExpressionSize cfg)
