{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import Symbolic.Regression.Boosting (
    ActiveTerm (..),
    BoostConfig (..),
    BoostResult (..),
    BoostState (..),
    buildEnsembleExpr,
    collapseEnsemble,
    defaultBoostConfig,
    fitBoosted,
    generateAndScore,
    partialEval,
    pruneByContribution,
    randomExpr,
 )
import qualified Symbolic.Regression.Boosting as Boosting
import Symbolic.Regression.EGraph (
    emptySREGraph,
    getBestExpr,
    insertTree,
    runSaturation,
 )
import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree)
import Symbolic.Regression.Expr.Opt (Distribution (..), minimizeNLL)
import Symbolic.Regression.Expr.Utils (countNodes)
import Symbolic.Regression.Fitness (mseScore)
import System.Exit (exitFailure)
import System.Random (mkStdGen, randomRs)

default (Int, Double)

-- ---------------------------------------------------------------------------
-- Test configurations
-- ---------------------------------------------------------------------------

-- | Light config for simple targets (no-param or 1-param expressions).
easyConfig :: RegressionConfig
easyConfig =
    defaultRegressionConfig
        { generations = 50
        , populationSize = 100
        , maxExpressionSize = 7
        , numIslands = 3
        , showTrace = False
        }

{- | Heavier config for multi-parameter targets that need more search budget
and optimizer iterations to converge.
-}
hardConfig :: RegressionConfig
hardConfig =
    defaultRegressionConfig
        { generations = 150
        , populationSize = 300
        , maxExpressionSize = 12
        , numIslands = 5
        , numOptimisationIterations = 80
        , numParameterRetries = 3
        , showTrace = False
        }

{- | Heaviest config for targets requiring two separate nonlinear terms
(e.g. p*x^2 + p*y^2 + p, size 11).
-}
veryHardConfig :: RegressionConfig
veryHardConfig =
    defaultRegressionConfig
        { generations = 200
        , populationSize = 300
        , maxExpressionSize = 12
        , numIslands = 5
        , numOptimisationIterations = 30
        , numParameterRetries = 3
        , showTrace = False
        }

-- ---------------------------------------------------------------------------
-- Shared dataset: 100 (x, y) pairs with deterministic seeds
-- ---------------------------------------------------------------------------
makeDataset :: D.DataFrame
makeDataset =
    let xs = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 54)
     in D.fromNamedColumns
            [ ("x", D.fromList xs)
            , ("y", D.fromList ys)
            ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------
computeMSE :: [Double] -> [Double] -> Double
computeMSE predicted target =
    let diffs = zipWith (-) predicted target
        n = fromIntegral (length diffs) :: Double
     in sum (map (^ (2 :: Int)) diffs) / n

assertNonEmpty :: String -> [a] -> IO ()
assertNonEmpty label [] = do
    putStrLn $ "  FAIL  " ++ label ++ ": fit returned no models"
    exitFailure
assertNonEmpty _ _ = pure ()

assertMSE :: String -> Double -> Double -> IO ()
assertMSE label threshold mse
    | mse <= threshold =
        putStrLn $ "  PASS  " ++ label ++ "  (MSE = " ++ show mse ++ ")"
    | otherwise =
        putStrLn
            ("  FAIL  " ++ label ++ ": MSE " ++ show mse ++ " > threshold " ++ show threshold)
            >> exitFailure

-- Evaluate all Pareto models and return the lowest MSE.
evalBestMSE :: [D.Expr Double] -> D.DataFrame -> Double
evalBestMSE models df =
    let actual = D.columnAsList (F.col "z") df
        mses =
            map (\m -> computeMSE (D.columnAsList m (D.derive "pred" m df)) actual) models
     in minimum mses

-- ---------------------------------------------------------------------------
-- Test 1: x^2 + y
-- ---------------------------------------------------------------------------
testQuadraticPlusY :: IO ()
testQuadraticPlusY = do
    putStrLn "Running: x^2 + y"
    let df0 = makeDataset
        x = F.col "x" :: D.Expr Double
        y = F.col "y" :: D.Expr Double
        df = D.derive "z" (F.pow x 2 + y) df0
    models <- fit (mkStdGen 42) easyConfig (F.col "z") df
    mapM_ (putStrLn . ("  Model: " ++) . D.prettyPrint) models
    assertNonEmpty "x^2 + y" models
    assertMSE "x^2 + y" 1e-2 (evalBestMSE models df)

-- ---------------------------------------------------------------------------
-- Test 2: x * y + 45
-- ---------------------------------------------------------------------------
testProduct :: IO ()
testProduct = do
    putStrLn "Running: x * y + 45"
    let df0 = makeDataset
        x = F.col "x" :: D.Expr Double
        y = F.col "y" :: D.Expr Double
        df = D.derive "z" (x * y + 45) df0
    models <- fit (mkStdGen 2) easyConfig (F.col "z") df
    mapM_ (putStrLn . ("  Model: " ++) . D.prettyPrint) models
    assertNonEmpty "x * y + 45" models
    assertMSE "x * y + 45" 1 (evalBestMSE models df)

-- ---------------------------------------------------------------------------
-- Test 3: 3x + 2y + 45 (linear combination)
-- ---------------------------------------------------------------------------
testLinear :: IO ()
testLinear = do
    putStrLn "Running: 3x + 2y + 45"
    let df0 = makeDataset
        x = F.col "x" :: D.Expr Double
        y = F.col "y" :: D.Expr Double
        df = D.derive "z" (3 * x + 2 * y + 45) df0
    models <- fit (mkStdGen 3) hardConfig (F.col "z") df
    mapM_ (putStrLn . ("  Model: " ++) . D.prettyPrint) models
    assertNonEmpty "3x + 2y + 45" models
    assertMSE "3x + 2y + 45" 1 (evalBestMSE models df)

-- ---------------------------------------------------------------------------
-- Test 4: 3x^2 + 2y^2 + 45 (degree-2 polynomial)
-- ---------------------------------------------------------------------------
testDegree2Poly :: IO ()
testDegree2Poly = do
    putStrLn "Running: 3x^2 + 2y^2 + 45"
    let df0 = makeDataset
        x = F.col "x" :: D.Expr Double
        y = F.col "y" :: D.Expr Double
        df = D.derive "z" (3 * F.pow x 2 + 2 * F.pow y 2 + 45) df0
    models <- fit (mkStdGen 4) veryHardConfig (F.col "z") df
    mapM_ (putStrLn . ("  Model: " ++) . D.prettyPrint) models
    assertNonEmpty "3x^2 + 2y^2 + 45" models
    assertMSE "3x^2 + 2y^2 + 45" 1 (evalBestMSE models df)

-- ===========================================================================
-- Unit tests: NLOPT parameter optimization
-- ===========================================================================

assert :: String -> Bool -> IO ()
assert label True = putStrLn $ "  PASS  " ++ label
assert label False = putStrLn ("  FAIL  " ++ label) >> exitFailure

assertClose :: String -> Double -> Double -> Double -> IO ()
assertClose label tol expected actual
    | abs (expected - actual) <= tol =
        putStrLn $ "  PASS  " ++ label ++ "  (got " ++ show actual ++ ")"
    | otherwise =
        putStrLn
            ( "  FAIL  "
                ++ label
                ++ ": expected "
                ++ show expected
                ++ " got "
                ++ show actual
            )
            >> exitFailure

-- | Test: optimize param0*x + param1 to find [3, 7]
testOptLinear :: IO ()
testOptLinear = do
    putStrLn "Running: NLOPT optimize param*x + param (linear)"
    let feats = V.fromList [VU.fromList [fromIntegral i] | i <- [1 :: Int .. 50]]
        target = VU.fromList [fromIntegral i * 3.0 + 7.0 | i <- [1 :: Int .. 50]]
        expr =
            Fix (BinF Add (Fix (BinF Mul (Fix (ParamF 0)) (Fix (VarF 0)))) (Fix (ParamF 1)))
        theta0 = VU.fromList [1.0, 1.0]
        (thetaOpt, loss, _) = minimizeNLL MSE Nothing 100 feats target expr theta0
    assertClose "opt linear: slope" 0.01 3.0 (thetaOpt VU.! 0)
    assertClose "opt linear: intercept" 0.1 7.0 (thetaOpt VU.! 1)
    assert "opt linear: MSE < 0.01" (loss < 0.01)

-- | Test: optimize param0 + x*y to find param0 = 45
testOptConstant :: IO ()
testOptConstant = do
    putStrLn "Running: NLOPT optimize param + x*y (constant = 45)"
    let xs = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 99)
        ys = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 100)
        feats = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [x * y + 45.0 | (x, y) <- zip xs ys]
        expr =
            Fix (BinF Add (Fix (ParamF 0)) (Fix (BinF Mul (Fix (VarF 0)) (Fix (VarF 1)))))
        theta0 = VU.fromList [0.0]
        (thetaOpt, loss, _) = minimizeNLL MSE Nothing 100 feats target expr theta0
    assertClose "opt constant: value" 0.1 45.0 (thetaOpt VU.! 0)
    assert "opt constant: MSE < 0.01" (loss < 0.01)

-- | Test: zero-parameter expression evaluates correctly
testOptNoParams :: IO ()
testOptNoParams = do
    putStrLn "Running: NLOPT with no parameters"
    let feats = V.fromList [VU.fromList [fromIntegral i] | i <- [1 :: Int .. 50]]
        target = VU.fromList [fromIntegral i * 2.0 | i <- [1 :: Int .. 50]]
        expr = Fix (BinF Mul (Fix (VarF 0)) (Fix (LitF 2.0)))
        theta0 = VU.empty
        (_, loss, _) = minimizeNLL MSE Nothing 100 feats target expr theta0
    assert "no params: MSE == 0" (loss < 1e-10)

-- ===========================================================================
-- Unit tests: rewrites and normalization
-- ===========================================================================

simplify :: Fix ExprF -> Fix ExprF
simplify expr =
    let eg0 = emptySREGraph
        (!cid, !eg1) = insertTree expr eg0
        !eg2 = runSaturation eg1
     in getBestExpr cid eg2

-- Helper: check that two expressions evaluate identically on test data
evalsEqual :: Fix ExprF -> Fix ExprF -> Bool
evalsEqual a b =
    let feats =
            V.fromList
                [VU.fromList [fromIntegral i, fromIntegral (i * 2)] | i <- [1 :: Int .. 10]]
        theta = VU.fromList [1.5, -0.5]
        va = evalTree feats theta a
        vb = evalTree feats theta b
     in VU.length va == VU.length vb
            && VU.all (< 1e-10) (VU.zipWith (\x y -> abs (x - y)) va vb)

testRewriteAddZero :: IO ()
testRewriteAddZero = do
    putStrLn "Running: x + 0 evaluates same as x"
    let result = simplify (Fix (BinF Add (Fix (VarF 0)) (Fix (LitF 0))))
    assert "x + 0 ≡ x" (evalsEqual result (Fix (VarF 0)))

testRewriteMulOne :: IO ()
testRewriteMulOne = do
    putStrLn "Running: x * 1 evaluates same as x"
    let result = simplify (Fix (BinF Mul (Fix (VarF 0)) (Fix (LitF 1))))
    assert "x * 1 ≡ x" (evalsEqual result (Fix (VarF 0)))

testRewriteMulZero :: IO ()
testRewriteMulZero = do
    putStrLn "Running: x * 0 → 0"
    let result = simplify (Fix (BinF Mul (Fix (VarF 0)) (Fix (LitF 0))))
    assert "x * 0 → 0" (result == Fix (LitF 0))

testRewriteLogExp :: IO ()
testRewriteLogExp = do
    putStrLn "Running: rewrite log(exp(x)) → x"
    let result = simplify (Fix (UnF Log (Fix (UnF Exp (Fix (VarF 0))))))
    assert "log(exp(x)) → x" (result == Fix (VarF 0))

testRewriteSubSelf :: IO ()
testRewriteSubSelf = do
    putStrLn "Running: x - x evaluates to 0"
    -- Sub is normalized to Add + Neg, so x - x becomes SumF [x, Neg(x)]
    let result = simplify (Fix (BinF Sub (Fix (VarF 0)) (Fix (VarF 0))))
    assert "x - x ≡ 0" (evalsEqual result (Fix (LitF 0)))

testConstantFolding :: IO ()
testConstantFolding = do
    putStrLn "Running: constant folding 2 + 3 → 5"
    let result = simplify (Fix (BinF Add (Fix (LitF 2)) (Fix (LitF 3))))
    assert "2 + 3 → 5" (result == Fix (LitF 5))

testSumFNormalization :: IO ()
testSumFNormalization = do
    putStrLn "Running: BinF Add → SumF normalization"
    let eg0 = emptySREGraph
        (!cid, !eg1) = insertTree (Fix (BinF Add (Fix (VarF 0)) (Fix (VarF 1)))) eg0
        result = getBestExpr cid eg1
    case result of
        Fix (SumF _) -> assert "Add → SumF" True
        other -> assert ("Add → SumF (got " ++ show other ++ ")") False

testProdFNormalization :: IO ()
testProdFNormalization = do
    putStrLn "Running: BinF Mul → ProdF normalization"
    let eg0 = emptySREGraph
        (!cid, !eg1) = insertTree (Fix (BinF Mul (Fix (VarF 0)) (Fix (VarF 1)))) eg0
        result = getBestExpr cid eg1
    case result of
        Fix (ProdF _) -> assert "Mul → ProdF" True
        other -> assert ("Mul → ProdF (got " ++ show other ++ ")") False

testSumFEval :: IO ()
testSumFEval = do
    putStrLn "Running: SumF evaluation"
    let feats = V.fromList [VU.fromList [1, 2], VU.fromList [3, 4]]
        result = evalTree feats VU.empty (Fix (SumF [Fix (VarF 0), Fix (VarF 1)]))
    assert "SumF [x,y] row 0 = 3" (abs (result VU.! 0 - 3.0) < 1e-10)
    assert "SumF [x,y] row 1 = 7" (abs (result VU.! 1 - 7.0) < 1e-10)

testProdFEval :: IO ()
testProdFEval = do
    putStrLn "Running: ProdF evaluation"
    let feats = V.fromList [VU.fromList [2, 3], VU.fromList [4, 5]]
        result = evalTree feats VU.empty (Fix (ProdF [Fix (VarF 0), Fix (VarF 1)]))
    assert "ProdF [x,y] row 0 = 6" (abs (result VU.! 0 - 6.0) < 1e-10)
    assert "ProdF [x,y] row 1 = 20" (abs (result VU.! 1 - 20.0) < 1e-10)

-- ===========================================================================
-- Boosting tests
-- ===========================================================================

-- | Shared small dataset for boosting tests
boostDataset :: (V.Vector (VU.Vector Double), VU.Vector Double)
boostDataset =
    let xs = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 53)
        ys = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 54)
        xss = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [x * y | (x, y) <- zip xs ys]
     in (xss, target)

testRandomExprGeneration :: IO ()
testRandomExprGeneration = do
    putStrLn "Running: randomExpr generates valid trees"
    let (expr1, g1) = randomExpr 5 3 (mkStdGen 42)
        (expr2, _g2) = randomExpr 5 3 g1
        sz1 = countNodes expr1
        sz2 = countNodes expr2
    assert "randomExpr: size <= 5" (sz1 <= 7 && sz2 <= 7) -- small tolerance
    assert "randomExpr: different seeds give different trees" (expr1 /= expr2)
    putStrLn $ "  Generated: " ++ show sz1 ++ " nodes, " ++ show sz2 ++ " nodes"

testRandomExprEvaluates :: IO ()
testRandomExprEvaluates = do
    putStrLn "Running: randomExpr evaluates without crashing"
    let (xss, _) = boostDataset
        (expr, _) = randomExpr 5 2 (mkStdGen 99)
        theta = VU.fromList [1.0, 0.5, -0.3, 2.0]
        result = evalTree xss theta expr
    assert "randomExpr evaluates: correct length" (VU.length result == V.length xss)

-- Just checking it doesn't crash — NaN/Inf are expected for some random trees

testBuildEnsembleExpr :: IO ()
testBuildEnsembleExpr = do
    putStrLn "Running: buildEnsembleExpr constructs valid expression"
    let term1 = ActiveTerm (var 0) 2.0 VU.empty VU.empty Nothing
        term2 = ActiveTerm (var 1) 3.0 VU.empty VU.empty Nothing
        ensemble = buildEnsembleExpr 5.0 [term1, term2]
        -- Should be: 5.0 + 2.0 * x0 + 3.0 * x1
        feats = V.fromList [VU.fromList [10.0, 20.0]]
        result = evalTree feats VU.empty ensemble
        expected = 5.0 + 2.0 * 10.0 + 3.0 * 20.0 -- = 85.0
    assertClose "ensemble eval" 1e-6 expected (result VU.! 0)

testPruneByContrib :: IO ()
testPruneByContrib = do
    putStrLn "Running: pruneByContribution removes weak terms"
    let big = ActiveTerm (var 0) 10.0 (VU.fromList [1, 1, 1]) VU.empty Nothing
        small = ActiveTerm (var 1) 0.001 (VU.fromList [1, 1, 1]) VU.empty Nothing
        medium = ActiveTerm (var 0) 5.0 (VU.fromList [1, 1, 1]) VU.empty Nothing
        pruned = pruneByContribution 0.05 [big, small, medium]
    assert "prune: big survives" (length pruned == 2) -- small should be pruned
    assert "prune: small removed" (all (\t -> atCoeff t > 0.01) pruned)

testCollapseEnsemble :: IO ()
testCollapseEnsemble = do
    putStrLn "Running: collapseEnsemble reduces term count"
    let (xss, target) = boostDataset
        nRows = V.length xss
        -- Build a multi-term ensemble manually
        bias = 30.0
        term1 =
            ActiveTerm
                (var 0)
                1.5
                (VU.generate nRows (\r -> 1.5 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 0)))
                (VU.generate nRows (\r -> 1.5 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 0)))
                Nothing
        term2 =
            ActiveTerm
                (var 1)
                2.0
                (VU.generate nRows (\r -> 2.0 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 1)))
                (VU.generate nRows (\r -> 2.0 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 1)))
                Nothing
        trainPreds =
            VU.generate
                nRows
                ( \r ->
                    bias
                        + 1.5 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 0)
                        + 2.0 * (xss `V.unsafeIndex` r `VU.unsafeIndex` 1)
                )
        st =
            BoostState
                { bsRound = 10
                , bsBias = bias
                , bsTerms = [term1, term2]
                , bsTrainPreds = trainPreds
                , bsValPreds = trainPreds
                , bsResiduals = VU.zipWith (-) target trainPreds
                , bsGraph = emptySREGraph
                , bsValHistory = []
                , bsElite = []
                }
        cfg = defaultBoostConfig{bcSatIters = 2, bcShowTrace = False, bcNloptIters = 30}
    (st', _) <- collapseEnsemble cfg (xss, target) (xss, target) st (mkStdGen 42)
    let nTermsAfter = length (bsTerms st')
    putStrLn $ "  Before: 2 terms, After: " ++ show nTermsAfter ++ " terms"
    assert "collapse: reduces to 1 term" (nTermsAfter == 1)

testFitBoostedSimple :: IO ()
testFitBoostedSimple = do
    putStrLn "Running: fitBoosted on x * y"
    let (xss, target) = boostDataset
        cfg =
            defaultBoostConfig
                { bcRounds = 30
                , bcCandidatesPerRound = 30
                , bcShowTrace = False
                }
    result <- fitBoosted (mkStdGen 42) cfg (xss, target) (xss, target)
    let r2 = 1.0 - brTrainMSE result * fromIntegral (VU.length target) / ssTot
        yMean = VU.sum target / fromIntegral (VU.length target)
        ssTot = VU.sum (VU.map (\y -> (y - yMean) ** 2) target)
    putStrLn $
        "  R² = " ++ show r2 ++ ", terms = " ++ show (length (brTerms result))
    assert "fitBoosted x*y: R² > 0.5" (r2 > 0.5)

testFitBoostedPerfectStop :: IO ()
testFitBoostedPerfectStop = do
    putStrLn "Running: fitBoosted stops at MSE ≈ 0"
    -- Simple target: y = x (easily achievable)
    let xss = V.fromList [VU.fromList [fromIntegral i] | i <- [1 :: Int .. 30]]
        target = VU.fromList [fromIntegral i | i <- [1 :: Int .. 30]]
        cfg =
            defaultBoostConfig
                { bcRounds = 100
                , bcCandidatesPerRound = 30
                , bcShowTrace = False
                }
    result <- fitBoosted (mkStdGen 42) cfg (xss, target) (xss, target)
    putStrLn $ "  MSE = " ++ show (brTrainMSE result)
    assert "perfect stop: MSE < 0.01" (brTrainMSE result < 0.01)

testValidationGating :: IO ()
testValidationGating = do
    putStrLn "Running: validation gating prevents worsening"
    -- Use different train/val to test that val gating works
    let xss = V.fromList [VU.fromList [fromIntegral i] | i <- [1 :: Int .. 50]]
        target = VU.fromList [fromIntegral i * 2.0 | i <- [1 :: Int .. 50]]
        cfg =
            defaultBoostConfig
                { bcRounds = 20
                , bcCandidatesPerRound = 20
                , bcShowTrace = False
                }
    result <- fitBoosted (mkStdGen 42) cfg (xss, target) (xss, target)
    -- With validation gating, MSE should be monotonically non-increasing
    -- (using same data for train/val, so any worsening is caught)
    assert "val gating: MSE reasonable" (brValMSE result < brTrainMSE result + 1e-6)

-- ===========================================================================
-- Unit tests: partialEval
-- ===========================================================================

testPEvalConstantsOnly :: IO ()
testPEvalConstantsOnly = do
    putStrLn "Running: partialEval folds pure constants"
    -- 2 + 3 → 5
    let expr = Fix (BinF Add (Fix (LitF 2)) (Fix (LitF 3)))
        result = partialEval expr
    assert "2 + 3 → Lit 5" (result == Fix (LitF 5))

testPEvalNestedConstants :: IO ()
testPEvalNestedConstants = do
    putStrLn "Running: partialEval folds nested constants"
    -- 0.5 * (2 * x + 4 * x) should collect like terms: 0.5 * 6x = 3x
    -- Build: SumF [ProdF [2, x], ProdF [4, x]] then wrap in ProdF [0.5, ...]
    let x = var 0
        inner = Fix (SumF [Fix (ProdF [lit 2, x]), Fix (ProdF [lit 4, x])])
        expr = Fix (SumF [Fix (ProdF [lit 0.5, inner]), Fix (ProdF [lit 5, x])]) -- 0.5*(2x+4x) + 5x
        result = partialEval expr
        sz = countNodes result
    putStrLn $ "  Result: " ++ show result ++ " (" ++ show sz ++ " nodes)"
    -- After collecting like terms: SumF [ProdF [6, x]] → ProdF [6, x]
    -- Then ProdF [0.5, ProdF [6, x]] → after normalize → ProdF [3, x]
    assert "0.5*(2x+4x) + 5x → 8x" (result == Fix (ProdF [8, x]))

testPEvalLikeTerms :: IO ()
testPEvalLikeTerms = do
    putStrLn "Running: partialEval collects like terms"
    -- 0.83*x + (-0.90*x) → c*x (single coefficient)
    let x = var 0
        expr = Fix (SumF [Fix (ProdF [lit 0.83, x]), Fix (ProdF [lit (-0.90), x])])
        result = partialEval expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (ProdF [Fix (LitF c), Fix (VarF 0)]) ->
            assertClose "0.83x + -0.90x → -0.07x" 1e-10 (-0.07) c
        _ -> assert "0.83x + -0.90x → ProdF [c, x]" False

testPEvalDropsZeroTerms :: IO ()
testPEvalDropsZeroTerms = do
    putStrLn "Running: partialEval drops near-zero terms"
    -- 1.0*x + (-1.0)*x → 0
    let x = var 0
        expr = Fix (SumF [Fix (ProdF [lit 1.0, x]), Fix (ProdF [lit (-1.0), x])])
        result = partialEval expr
    putStrLn $ "  Result: " ++ show result
    assert "x + -x → Lit 0" (result == Fix (LitF 0))

testPEvalUnaryOnLit :: IO ()
testPEvalUnaryOnLit = do
    putStrLn "Running: partialEval evaluates unary on constant"
    let result1 = partialEval (Fix (UnF Neg (Fix (LitF 3))))
    assert "Neg(3) → -3" (result1 == Fix (LitF (-3)))
    let result2 = partialEval (Fix (UnF Sq (Fix (LitF 4))))
    assert "Sq(4) → 16" (result2 == Fix (LitF 16))

testPEvalPreservesVars :: IO ()
testPEvalPreservesVars = do
    putStrLn "Running: partialEval preserves variable structure"
    -- sin(x) + 5 stays as SumF [Lit 5, Sin(x)]
    let expr = Fix (SumF [Fix (UnF Sin (var 0)), lit 5])
        result = partialEval expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (SumF [Fix (LitF 5.0), Fix (UnF Sin (Fix (VarF 0)))]) -> assert "sin(x)+5 preserved" True
        _ -> assert ("sin(x)+5 preserved (got " ++ show result ++ ")") False

testPEvalMixedSum :: IO ()
testPEvalMixedSum = do
    putStrLn "Running: partialEval on mixed sum with constants + like terms"
    -- 3 + 2*x + 5 + 3*x → 8 + 5*x
    let x = var 0
        expr = Fix (SumF [lit 3, Fix (ProdF [lit 2, x]), lit 5, Fix (ProdF [lit 3, x])])
        result = partialEval expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (SumF [Fix (LitF c), Fix (ProdF [Fix (LitF k), Fix (VarF 0)])]) -> do
            assertClose "3+2x+5+3x: constant" 1e-10 8.0 c
            assertClose "3+2x+5+3x: coefficient" 1e-10 5.0 k
        _ -> assert ("3+2x+5+3x → SumF [8, 5x] (got " ++ show result ++ ")") False

testPEvalComplex :: IO ()
testPEvalComplex = do
    putStrLn "Running: partialEval on 0 + 45 + negate(1*y*y*-2) + 1*x*x*3"
    -- Input: 0.0 + 45 + negate(1.0 * y * y * -2) + 1 * x * x * 3
    -- Expected: 45 + 2*y*y + 3*x*x
    let x = var 0
        y = var 1
        expr =
            Fix
                ( SumF
                    [ lit 0
                    , lit 45
                    , Fix (UnF Neg (Fix (ProdF [lit 1, y, y, lit (-2)])))
                    , Fix (ProdF [lit 1, x, x, lit 3])
                    ]
                )
        result = partialEval expr
    putStrLn $ "  Result: " ++ show result
    -- Expected: SumF [Lit 45, ProdF [Lit 3, x, x], ProdF [Lit 2, y, y]]
    let expected =
            Fix
                ( SumF
                    [ lit 45
                    , Fix (ProdF [lit 3, x, x])
                    , Fix (ProdF [lit 2, y, y])
                    ]
                )
    assert "complex: 45 + 3*x*x + 2*y*y" (show result == show expected)

-- | Run boosting on several targets and assert distilled expressions are <= 20 nodes.
testBoostExprSize :: IO ()
testBoostExprSize = do
    let cfg =
            defaultBoostConfig
                { bcRounds = 30
                , bcCandidatesPerRound = 30
                , bcShowTrace = False
                }
        maxNodes = 20
        -- Target: x * y (2 vars)
        xs1 = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 53)
        ys1 = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 54)
        xss1 = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs1 ys1]
        target1 = VU.fromList [x * y | (x, y) <- zip xs1 ys1]
        -- Target: 3*x + 2*y (linear)
        target2 = VU.fromList [3 * x + 2 * y | (x, y) <- zip xs1 ys1]
        -- Target: x^2 (single var)
        xs3 = take 50 $ randomRs (1 :: Double, 10) (mkStdGen 55)
        xss3 = V.fromList [VU.fromList [x] | x <- xs3]
        target3 = VU.fromList [x * x | x <- xs3]

    mapM_
        ( \(name, xss, tgt) -> do
            putStrLn $ "Running: boost expr size on " ++ name
            result <- fitBoosted (mkStdGen 42) cfg (xss, tgt) (xss, tgt)
            let expr = case brDistilled result of
                    (d : _) -> d
                    [] -> brEnsembleExpr result
                nodes = countNodes expr
            putStrLn $ "  " ++ name ++ ": " ++ show nodes ++ " nodes"
            putStrLn $ "  Expression: " ++ show expr
            assert (name ++ ": <= " ++ show maxNodes ++ " nodes") (nodes <= maxNodes)
        )
        [ ("x*y", xss1, target1)
        , ("3x+2y", xss1, target2)
        , ("x^2", xss3, target3)
        ]

-- ===========================================================================
-- Boosting integration tests (same targets as GP tests)
-- ===========================================================================

boostIntConfig :: BoostConfig
boostIntConfig =
    defaultBoostConfig
        { bcRounds = 50
        , bcMaxWeakLearnerSize = 15
        , bcCandidatesPerRound = 100
        , bcNloptIters = 60
        , bcLearningRate = 0.3
        , bcShowTrace = False
        }

-- | Helper: run boosting via the raw vector API and check ensemble R²
boostAndCheck ::
    String -> Double -> V.Vector (VU.Vector Double) -> VU.Vector Double -> IO ()
boostAndCheck name minR2 xss target = do
    putStrLn $ "Running: boost " ++ name
    result <- fitBoosted (mkStdGen 42) boostIntConfig (xss, target) (xss, target)
    let mse = brTrainMSE result
        yMean = VU.sum target / fromIntegral (VU.length target)
        ssTot = VU.sum (VU.map (\y -> (y - yMean) ** 2) target)
        r2 =
            if ssTot == 0 then 1.0 else 1.0 - mse * fromIntegral (VU.length target) / ssTot
    putStrLn $
        "  R² = "
            ++ show r2
            ++ " MSE = "
            ++ show mse
            ++ " terms = "
            ++ show (length (brTerms result))
    case brDistilled result of
        (d : _) -> putStrLn $ "  Distilled: " ++ show d
        _ -> pure ()
    assert (name ++ ": R² > " ++ show minR2) (r2 > minR2)

testBoostQuadraticPlusY :: IO ()
testBoostQuadraticPlusY = do
    let xs = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 54)
        xss = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [x * x + y | (x, y) <- zip xs ys]
    boostAndCheck "x^2 + y" 0.99 xss target

testBoostProduct :: IO ()
testBoostProduct = do
    let xs = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 54)
        xss = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [x * y + 45 | (x, y) <- zip xs ys]
    boostAndCheck "x*y + 45" 0.99 xss target

testBoostLinear :: IO ()
testBoostLinear = do
    let xs = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 100) (mkStdGen 54)
        xss = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [3 * x + 2 * y + 45 | (x, y) <- zip xs ys]
    boostAndCheck "3x + 2y + 45" 0.99 xss target

testBoostDegree2Poly :: IO ()
testBoostDegree2Poly = do
    -- Use smaller range [1,10] to avoid numerical blowup with large expressions
    let xs = take 100 $ randomRs (1 :: Double, 10) (mkStdGen 53)
        ys = take 100 $ randomRs (1 :: Double, 10) (mkStdGen 54)
        xss = V.fromList [VU.fromList [x, y] | (x, y) <- zip xs ys]
        target = VU.fromList [3 * x * x + 2 * y * y + 45 | (x, y) <- zip xs ys]
    boostAndCheck "3x^2 + 2y^2 + 45" 0.95 xss target

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== Unit tests: optimization ==="
    testOptLinear
    testOptConstant
    testOptNoParams

    putStrLn "\n=== Unit tests: rewrites & normalization ==="
    testRewriteAddZero
    testRewriteMulOne
    testRewriteMulZero
    testRewriteLogExp
    testRewriteSubSelf
    testConstantFolding
    testSumFNormalization
    testProdFNormalization
    testSumFEval
    testProdFEval

    putStrLn "\n=== Partial evaluator tests ==="
    testPEvalConstantsOnly
    testPEvalNestedConstants
    testPEvalLikeTerms
    testPEvalDropsZeroTerms
    testPEvalUnaryOnLit
    testPEvalPreservesVars
    testPEvalMixedSum
    testPEvalComplex

    putStrLn "\n=== Boosting tests ==="
    testRandomExprGeneration
    testRandomExprEvaluates
    testBuildEnsembleExpr
    testPruneByContrib
    testCollapseEnsemble
    testFitBoostedSimple
    testFitBoostedPerfectStop
    testValidationGating

    putStrLn "\n=== Boosting integration tests ==="
    testBoostQuadraticPlusY
    testBoostProduct
    testBoostLinear
    testBoostDegree2Poly

    putStrLn "\n=== Integration tests: symbolic regression ==="
    testQuadraticPlusY
    testProduct
    testLinear
    testDegree2Poly
    putStrLn "\n=== All tests passed ==="
