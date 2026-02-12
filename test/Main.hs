{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Symbolic.Regression
import Test.HUnit

-- | Default regression config for testing: disabled tracing and defined seed for deterministic rng
defaultTestConfig :: RegressionConfig
defaultTestConfig =
    defaultRegressionConfig
        { showTrace = False
        , seed = Just 0
        }

-- | Test linear formula regression (y = 2x + 1)
testLinearFormula :: Test
testLinearFormula = TestCase $ do
    let xs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0] :: [Double]
        ys = map (\x -> 2 * x + 1) xs -- Known formula: y = 2x + 1
        df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
        target = F.col @Double "y"
        cfg =
            defaultTestConfig
                { generations = 30
                , populationSize = 80
                , maxExpressionSize = 4
                }

    exprs <- fit cfg target df

    assertBool "Should find at least one expression" (not $ null exprs)

    -- Test accuracy: the best expression should closely match y = 2x + 1
    let bestExpr = last exprs -- Most complex/accurate should be last
        predDf = D.derive "predicted" bestExpr df
        actualVals = D.columnAsList (F.col @Double "y") predDf :: [Double]
        predVals = D.columnAsList (F.col @Double "predicted") predDf :: [Double]
        errorValues = zipWith (\a p -> abs (a - p)) actualVals predVals
        maxError = maximum errorValues

    -- For perfect linear data, should have reasonably low error
    assertBool ("Max error should be < 1.0, got: " ++ show maxError) (maxError < 1.0)

-- | Test constant formula regression (y = 42)
testConstantFormula :: Test
testConstantFormula = TestCase $ do
    let xs = [1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]
        ys = replicate 5 42.0 :: [Double] -- Constant target: y = 42
        df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
        target = F.col @Double "y"
        cfg =
            defaultTestConfig
                { generations = 3
                , populationSize = 10
                , maxExpressionSize = 1
                }

    exprs <- fit cfg target df
    assertBool "Should handle constant target without crashing" (not $ null exprs)

    -- Test that it can predict the constant correctly
    let bestExpr = last exprs
        predDf = D.derive "predicted" bestExpr df
        predVals = D.columnAsList (F.col @Double "predicted") predDf :: [Double]
        errorValues = map (\p -> abs (42.0 - p)) predVals
        maxError = maximum errorValues

    assertBool ("Should predict constant accurately, max error: " ++ show maxError) (maxError < 1.0)

-- | Test quadratic formula regression (y = x² + 2x + 3)
testQuadraticFormula :: Test
testQuadraticFormula = TestCase $ do
    let xs = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0] :: [Double]
        ys = map (\x -> x * x + 2 * x + 3) xs -- Known formula: y = x² + 2x + 3
        df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys)]
        target = F.col @Double "y"
        cfg =
            defaultTestConfig
                { generations = 50
                , populationSize = 150
                , maxExpressionSize = 8 -- Need more complexity for quadratic
                }

    exprs <- fit cfg target df

    assertBool "Should find at least one expression" (not $ null exprs)

    -- Test accuracy: the best expression should closely match y = x² + 2x + 3
    let bestExpr = last exprs -- Most complex/accurate should be last
        predDf = D.derive "predicted" bestExpr df
        actualVals = D.columnAsList (F.col @Double "y") predDf :: [Double]
        predVals = D.columnAsList (F.col @Double "predicted") predDf :: [Double]
        errorValues = zipWith (\a p -> abs (a - p)) actualVals predVals
        maxError = maximum errorValues

    -- Quadratic might need slightly higher tolerance than linear
    assertBool ("Max error should be < 0.5, got: " ++ show maxError) (maxError < 0.5)

-- | Test two-variable formula regression (z = x + y)
testTwoVariableFormula :: Test
testTwoVariableFormula = TestCase $ do
    let xs = [1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]
        ys = [2.0, 3.0, 1.0, 4.0, 2.0] :: [Double]
        zs = zipWith (+) xs ys -- Known formula: z = x + y
        df = D.fromNamedColumns [("x", D.fromList xs), ("y", D.fromList ys), ("z", D.fromList zs)]
        target = F.col @Double "z"
        cfg =
            defaultTestConfig
                { generations = 20
                , populationSize = 60
                , maxExpressionSize = 3
                }

    exprs <- fit cfg target df

    assertBool "Should find at least one expression" (not $ null exprs)

    -- Test accuracy: the best expression should closely match z = x + y
    let bestExpr = last exprs -- Most complex/accurate should be last
        predDf = D.derive "predicted" bestExpr df
        actualVals = D.columnAsList (F.col @Double "z") predDf :: [Double]
        predVals = D.columnAsList (F.col @Double "predicted") predDf :: [Double]
        errorValues = zipWith (\a p -> abs (a - p)) actualVals predVals
        maxError = maximum errorValues

    -- Two-variable linear should be quite accurate
    assertBool ("Max error should be < 0.1, got: " ++ show maxError) (maxError < 0.1)

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
