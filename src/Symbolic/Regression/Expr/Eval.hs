{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Tree evaluation on vectorized data.
module Symbolic.Regression.Expr.Eval (
    evalTree,
    evalBinOp,
    evalUnOp,
) where

import Symbolic.Regression.Expr

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{- | Evaluate an expression tree on a feature matrix with parameter vector.
Returns one prediction per row.
-}
evalTree :: Features -> PVector -> Fix ExprF -> PVector
evalTree xss theta = go
  where
    !nRows = V.length xss

    go (Fix node) = case node of
        VarF i -> VU.generate nRows (\r -> xss `V.unsafeIndex` r `VU.unsafeIndex` i)
        ParamF i -> VU.replicate nRows (theta `VU.unsafeIndex` i)
        LitF x -> VU.replicate nRows x
        BinF op l r -> VU.zipWith (evalBinOp op) (go l) (go r)
        UnF f c -> VU.map (evalUnOp f) (go c)
        SumF [] -> VU.replicate nRows 0
        SumF (x : xs) -> foldl' (\(!acc) t -> VU.zipWith (+) acc (go t)) (go x) xs
        ProdF [] -> VU.replicate nRows 1
        ProdF (x : xs) -> foldl' (\(!acc) t -> VU.zipWith (*) acc (go t)) (go x) xs
        PolyF _ -> error "evalTree: PolyF should not appear outside e-graph"
{-# INLINE evalTree #-}

-- | Evaluate a binary operation (with safe division).
evalBinOp :: BinOp -> Double -> Double -> Double
evalBinOp = \case
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> \l r -> if r == 0 then l else l / r
    Pow -> \l r -> if l == 0 && r < 0 then 0 else l ** r
{-# INLINE evalBinOp #-}

-- | Evaluate a unary operation (with safe log/sqrt/recip).
evalUnOp :: UnOp -> Double -> Double
evalUnOp = \case
    Neg -> negate
    Abs -> abs
    Recip -> \x -> if x == 0 then 0 else 1 / x
    Sq -> \x -> x * x
    Cube -> \x -> x * x * x
    Sqrt -> \x -> if x < 0 then sqrt (abs x) else sqrt x
    Exp -> exp . min 700
    Log -> \x -> if x <= 0 then log (abs x + 1e-300) else log x
    Sin -> sin
    Cos -> cos
{-# INLINE evalUnOp #-}
