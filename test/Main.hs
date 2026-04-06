{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import Symbolic.Regression.EGraph (
    emptySREGraph,
    getBestExpr,
    insertTree,
    runSaturation,
 )
import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree)
import Symbolic.Regression.Expr.Opt (Distribution (..), minimizeNLL)
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

    putStrLn "\n=== Integration tests: symbolic regression ==="
    testQuadraticPlusY
    testProduct
    testLinear
    testDegree2Poly
    putStrLn "\n=== All tests passed ==="
