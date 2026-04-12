{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
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
import Symbolic.Regression.Poly (polySimplify)
import System.Exit (exitFailure)
import System.Random (mkStdGen, randomRs)

default (Int, Double)

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
-- Entry point
-- ===========================================================================

-- ===========================================================================
-- Unit tests: polySimplify
-- ===========================================================================

testPEvalConstantsOnly :: IO ()
testPEvalConstantsOnly = do
    putStrLn "Running: polySimplify folds pure constants"
    let expr = Fix (BinF Add (Fix (LitF 2)) (Fix (LitF 3)))
        result = polySimplify expr
    assert "2 + 3 → Lit 5" (result == Fix (LitF 5))

testPEvalNestedConstants :: IO ()
testPEvalNestedConstants = do
    putStrLn "Running: polySimplify folds nested constants"
    let x = var 0
        inner = Fix (SumF [Fix (ProdF [lit 2, x]), Fix (ProdF [lit 4, x])])
        expr = Fix (SumF [Fix (ProdF [lit 0.5, inner]), Fix (ProdF [lit 5, x])])
        result = polySimplify expr
        sz = countNodes result
    putStrLn $ "  Result: " ++ show result ++ " (" ++ show sz ++ " nodes)"
    assert "0.5*(2x+4x) + 5x → 8x" (result == Fix (ProdF [8, x]))

testPEvalLikeTerms :: IO ()
testPEvalLikeTerms = do
    putStrLn "Running: polySimplify collects like terms"
    let x = var 0
        expr = Fix (SumF [Fix (ProdF [lit 0.83, x]), Fix (ProdF [lit (-0.90), x])])
        result = polySimplify expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (ProdF [Fix (LitF c), Fix (VarF 0)]) ->
            assertClose "0.83x + -0.90x → -0.07x" 1e-10 (-0.07) c
        _ -> assert "0.83x + -0.90x → ProdF [c, x]" False

testPEvalDropsZeroTerms :: IO ()
testPEvalDropsZeroTerms = do
    putStrLn "Running: polySimplify drops near-zero terms"
    let x = var 0
        expr = Fix (SumF [Fix (ProdF [lit 1.0, x]), Fix (ProdF [lit (-1.0), x])])
        result = polySimplify expr
    putStrLn $ "  Result: " ++ show result
    assert "x + -x → Lit 0" (result == Fix (LitF 0))

testPEvalUnaryOnLit :: IO ()
testPEvalUnaryOnLit = do
    putStrLn "Running: polySimplify evaluates unary on constant"
    let result1 = polySimplify (Fix (UnF Neg (Fix (LitF 3))))
    assert "Neg(3) → -3" (result1 == Fix (LitF (-3)))
    let result2 = polySimplify (Fix (UnF Sq (Fix (LitF 4))))
    assert "Sq(4) → 16" (result2 == Fix (LitF 16))

testPEvalPreservesVars :: IO ()
testPEvalPreservesVars = do
    putStrLn "Running: polySimplify preserves variable structure"
    let expr = Fix (SumF [Fix (UnF Sin (var 0)), lit 5])
        result = polySimplify expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (SumF [Fix (LitF 5.0), Fix (UnF Sin (Fix (VarF 0)))]) -> assert "sin(x)+5 preserved" True
        _ -> assert ("sin(x)+5 preserved (got " ++ show result ++ ")") False

testPEvalMixedSum :: IO ()
testPEvalMixedSum = do
    putStrLn "Running: polySimplify on mixed sum with constants + like terms"
    let x = var 0
        expr = Fix (SumF [lit 3, Fix (ProdF [lit 2, x]), lit 5, Fix (ProdF [lit 3, x])])
        result = polySimplify expr
    putStrLn $ "  Result: " ++ show result
    case result of
        Fix (SumF [Fix (LitF c), Fix (ProdF [Fix (LitF k), Fix (VarF 0)])]) -> do
            assertClose "3+2x+5+3x: constant" 1e-10 8.0 c
            assertClose "3+2x+5+3x: coefficient" 1e-10 5.0 k
        _ -> assert ("3+2x+5+3x → SumF [8, 5x] (got " ++ show result ++ ")") False

testPEvalComplex :: IO ()
testPEvalComplex = do
    putStrLn "Running: polySimplify on 0 + 45 + negate(1*y*y*-2) + 1*x*x*3"
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
        result = polySimplify expr
    putStrLn $ "  Result: " ++ show result
    let expected =
            Fix
                ( SumF
                    [ lit 45
                    , Fix (ProdF [lit 3, x, x])
                    , Fix (ProdF [lit 2, y, y])
                    ]
                )
    assert "complex: 45 + 3*x*x + 2*y*y" (show result == show expected)

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

    putStrLn "\n=== Polynomial simplifier tests ==="
    testPEvalConstantsOnly
    testPEvalNestedConstants
    testPEvalLikeTerms
    testPEvalDropsZeroTerms
    testPEvalUnaryOnLit
    testPEvalPreservesVars
    testPEvalMixedSum
    testPEvalComplex
