{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import System.Exit (exitFailure)
import System.Random (mkStdGen, randomRs)

default (Int, Double)

-- ---------------------------------------------------------------------------
-- Test configuration: reduced counts for reasonable runtime (~1-2 min total)
-- ---------------------------------------------------------------------------
testConfig :: RegressionConfig
testConfig =
    defaultRegressionConfig
        { generations = 50
        , populationSize = 50
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

-- Apply the most complex (last) model from the Pareto front and compute MSE
-- against the "z" target column.
evalBestMSE :: [D.Expr Double] -> D.DataFrame -> Double
evalBestMSE models df =
    let best = last models
        predicted = D.columnAsList best (D.derive "pred" best df)
        actual = D.columnAsList (F.col "z") df
     in computeMSE predicted actual

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
    models <- fit (mkStdGen 1) testConfig (F.col "z") df
    assertNonEmpty "x^2 + y" models
    assertMSE "x^2 + y" 1e4 (evalBestMSE models df)

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
    models <- fit (mkStdGen 2) testConfig (F.col "z") df
    assertNonEmpty "x * y + 45" models
    assertMSE "x * y + 45" 1e5 (evalBestMSE models df)

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
    models <- fit (mkStdGen 3) testConfig (F.col "z") df
    assertNonEmpty "3x + 2y + 45" models
    assertMSE "3x + 2y + 45" 1e3 (evalBestMSE models df)

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
    models <- fit (mkStdGen 4) testConfig (F.col "z") df
    assertNonEmpty "3x^2 + 2y^2 + 45" models
    assertMSE "3x^2 + 2y^2 + 45" 1e8 (evalBestMSE models df)

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== symbolic-regression integration tests ==="
    testQuadraticPlusY
    testProduct
    testLinear
    testDegree2Poly
    putStrLn "=== All tests passed ==="
