{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import Test.HUnit

-- Default regression config for deterministic testing
defaultTestConfig :: RegressionConfig
defaultTestConfig =
    defaultRegressionConfig
        { showTrace = False
        , seed = Just 0
        }

-- Compute maximum absolute error between target and predicted expression
maxAbsError :: D.Expr Double -> D.Expr Double -> D.DataFrame -> Double
maxAbsError target predicted df =
    let (predColExpr, df') = D.deriveWithExpr "__test_predicted" predicted df
        actualVals = D.columnAsList target df' :: [Double]
        predVals = D.columnAsList predColExpr df' :: [Double]
     in maximum (zipWith (\a p -> abs (a - p)) actualVals predVals)

{-

(predicted, predDf) = D.deriveWithExpr "predicted" bestExpr
predValues = D.columnAsList predicted predDf

-}

-- Extract best evolved expression (last in population history)
bestExpression :: RegressionConfig -> D.Expr Double -> D.DataFrame -> IO (Maybe (D.Expr Double))
bestExpression cfg target df = do
    exprs <- fit cfg target df
    pure $ if null exprs then Nothing else Just (last exprs)

-- High-level regression verification used by tests
verifyRegressionAccuracy ::
    String ->
    RegressionConfig ->
    D.Expr Double ->
    D.DataFrame ->
    Double ->
    Assertion
verifyRegressionAccuracy label cfg target df tolerance = do
    mBest <- bestExpression cfg target df
    case mBest of
        Nothing ->
            assertFailure noResultMsg
        Just bestExpr -> do
            let err = maxAbsError target bestExpr df
            assertBool (errorMsg err) (err < tolerance)
  where
    noResultMsg =
        label ++ ": regression returned no expressions"

    errorMsg err =
        label
            ++ ": expected max abs error < "
            ++ show tolerance
            ++ ", got "
            ++ show err

-- Linear: y = 2x + 1
testLinearFormula :: Test
testLinearFormula =
    TestCase $
        let xs = [1.0 .. 10.0] :: [Double]
            ys = map (\x -> 2 * x + 1) xs
            df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
            target = F.col @Double "y"
            cfg =
                defaultTestConfig
                    { generations = 30
                    , populationSize = 80
                    , maxExpressionSize = 4
                    }
         in verifyRegressionAccuracy "Linear Formula" cfg target df 1.0

-- Constant: y = 42
testConstantFormula :: Test
testConstantFormula =
    TestCase $
        let xs = [1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]
            ys = replicate 5 (42.0 :: Double)
            df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
            target = F.col @Double "y"
            cfg =
                defaultTestConfig
                    { generations = 3
                    , populationSize = 10
                    , maxExpressionSize = 1
                    }
         in verifyRegressionAccuracy "Constant Formula" cfg target df 1.0

-- Quadratic: y = x² + 2x + 3
testQuadraticFormula :: Test
testQuadraticFormula =
    TestCase $
        let xs = [1.0 .. 6.0] :: [Double]
            ys = map (\x -> x * x + 2 * x + 3) xs
            df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
            target = F.col @Double "y"
            cfg =
                defaultTestConfig
                    { generations = 50
                    , populationSize = 150
                    , maxExpressionSize = 8
                    }
         in verifyRegressionAccuracy "Quadratic Formula" cfg target df 0.5

-- Two variable: z = x + y
testTwoVariableFormula :: Test
testTwoVariableFormula =
    TestCase $
        let xs = [1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]
            ys = [2.0, 3.0, 1.0, 4.0, 2.0] :: [Double]
            zs = zipWith (+) xs ys
            df =
                D.fromNamedColumns
                    [ ("x", D.fromList xs)
                    , ("y", D.fromList ys)
                    , ("z", D.fromList zs)
                    ]
            target = F.col @Double "z"
            cfg =
                defaultTestConfig
                    { generations = 20
                    , populationSize = 60
                    , maxExpressionSize = 3
                    }
         in verifyRegressionAccuracy "Two-Variable Formula" cfg target df 0.1

allTests :: Test
allTests =
    TestList
        [ "Linear Formula" ~: testLinearFormula
        , "Constant Formula" ~: testConstantFormula
        , "Quadratic Formula" ~: testQuadraticFormula
        , "Two-Variable Formula" ~: testTwoVariableFormula
        ]

main :: IO ()
main = do
    putStrLn "Running symbolic regression tests..."
    testCounts <- runTestTT allTests
    putStrLn $ "Tests run: " ++ show (cases testCounts)
    putStrLn $ "Failures: " ++ show (failures testCounts)
    putStrLn $ "Errors: " ++ show (errors testCounts)
