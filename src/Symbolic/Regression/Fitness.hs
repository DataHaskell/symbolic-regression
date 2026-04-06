{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Fitness evaluation with parameter optimization.
module Symbolic.Regression.Fitness (
    DataSet,
    evaluateFitness,
    evaluateExpr,
    r2Score,
    mseScore,
) where

import qualified Data.Vector.Unboxed as VU
import System.Random (StdGen, randomR)

import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree)
import Symbolic.Regression.Expr.Opt (Distribution (..), minimizeNLL)
import Symbolic.Regression.Expr.Utils (countParams, relabelParams)

-- | A dataset: feature matrix and target vector.
type DataSet = (Features, PVector)

-- | Evaluate fitness with parameter optimization.
evaluateFitness ::
    Int -> Int -> DataSet -> Fix ExprF -> StdGen -> (Double, PVector, StdGen)
evaluateFitness nIter nRetries (xTrain, yTrain) tree0 g0 =
    let !tree = relabelParams tree0
        !nParams = countParams tree :: Int
     in if nParams == 0
            then
                let !yPred = evalTree xTrain VU.empty tree
                    !err = mseScore yTrain yPred
                    !fit = if isNaN err || isInfinite err then -1e18 else negate err
                 in (fit, VU.empty, g0)
            else
                let (!bestFit, !bestTheta, !gFinal) = tryRetries nRetries nParams nIter xTrain yTrain tree g0
                 in (bestFit, bestTheta, gFinal)

tryRetries ::
    Int ->
    Int ->
    Int ->
    Features ->
    PVector ->
    Fix ExprF ->
    StdGen ->
    (Double, PVector, StdGen)
tryRetries nRetries nParams nIter xTrain yTrain tree g0 = go nRetries g0 (-1e18) VU.empty
  where
    go 0 !g !bestFit !bestTheta = (bestFit, bestTheta, g)
    go !n !g !bestFit !bestTheta =
        let (!theta0, !g') = randomTheta nParams g
            (!thetaOpt, _, _) = minimizeNLL MSE Nothing nIter xTrain yTrain tree theta0
            !yPred = evalTree xTrain thetaOpt tree
            !err = mseScore yTrain yPred
            !fit = if isNaN err || isInfinite err then -1e18 else negate err
         in if fit > bestFit
                then go (n - 1) g' fit thetaOpt
                else go (n - 1) g' bestFit bestTheta

randomTheta :: Int -> StdGen -> (PVector, StdGen)
randomTheta n g0 =
    let (!lst, !gFinal) = go n g0
     in (VU.fromListN n lst, gFinal)
  where
    go 0 !g = ([], g)
    go !k !g =
        let (!v, !g') = randomR (-1.0 :: Double, 1.0) g
            (!rest, !gF) = go (k - 1) g'
         in (v : rest, gF)

-- | Evaluate an expression on features with given parameters.
evaluateExpr :: Fix ExprF -> PVector -> Features -> PVector
evaluateExpr tree theta xMatrix = evalTree xMatrix theta tree

-- | R^2 score.
r2Score :: PVector -> PVector -> Double
r2Score yTrue yPred =
    let !n = VU.length yTrue
        !yMean = VU.sum yTrue / fromIntegral n
        !ssTot = VU.sum $ VU.map (\y -> (y - yMean) ^ (2 :: Int)) yTrue
        !ssRes = VU.sum $ VU.zipWith (\yt yp -> (yt - yp) ^ (2 :: Int)) yTrue yPred
     in if ssTot == 0 then 0 else 1 - ssRes / ssTot

-- | MSE score.
mseScore :: PVector -> PVector -> Double
mseScore yTrue yPred =
    let !n = VU.length yTrue
        !ssRes = VU.sum $ VU.zipWith (\yt yp -> (yt - yp) ^ (2 :: Int)) yTrue yPred
     in ssRes / fromIntegral n
