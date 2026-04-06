{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
-- | Fitness evaluation with NLopt parameter optimization.
-- Uses srtree's evaluation and optimization machinery directly,
-- making fitness evaluation identical to the original symbolic-regression.
module Symbolic.Regression.Fitness
    ( DataSet
    , evaluateFitness
    , evaluateExpr
    , r2Score
    , mseScore
    ) where

import qualified Data.Massiv.Array as M
import Data.SRTree.Internal (SRTree(..), countParams)
import Data.SRTree (relabelParams)
import qualified Data.SRTree.Recursion as SR
import Data.SRTree.Eval (evalTree, PVector, SRMatrix)
import Algorithm.SRTree.Opt (minimizeNLL)
import Algorithm.SRTree.Likelihoods (Distribution(..))
import System.Random (StdGen, randomR)

-- | A dataset: feature matrix (rows x cols) and target vector
type DataSet = (SRMatrix, PVector)

-- | Evaluate fitness with NLopt parameter optimization.
-- Relabels parameters, generates random initial theta, optimizes via NLopt,
-- returns (negated MSE, optimized theta).
-- This matches srtree's fitnessFun from Algorithm.EqSat.SearchSR.
evaluateFitness :: Int       -- ^ NLopt iterations
                -> Int       -- ^ Number of retries with random inits
                -> DataSet   -- ^ Training data
                -> SR.Fix SRTree
                -> StdGen
                -> (Double, PVector, StdGen)
evaluateFitness nIter nRetries (xTrain, yTrain) tree0 g0 =
    let !tree = relabelParams tree0
        !nParams = countParams tree :: Int
    in if nParams == 0
       then -- No parameters to optimize, just evaluate directly
           let !yPred = M.compute (evalTree xTrain M.empty tree)
               !mse = mseScore yTrain yPred
               !fit = if isNaN mse || isInfinite mse then -1e18 else negate mse
           in (fit, M.empty, g0)
       else -- Optimize parameters via NLopt with multiple retries
           let (!bestFit, !bestTheta, !gFinal) = tryRetries nRetries nParams nIter xTrain yTrain tree g0
           in (bestFit, bestTheta, gFinal)

-- | Try multiple random initializations, keep the best
tryRetries :: Int -> Int -> Int -> SRMatrix -> PVector -> SR.Fix SRTree -> StdGen -> (Double, PVector, StdGen)
tryRetries nRetries nParams nIter xTrain yTrain tree g0 = go nRetries g0 (-1e18) M.empty
  where
    go 0 !g !bestFit !bestTheta = (bestFit, bestTheta, g)
    go !n !g !bestFit !bestTheta =
        let (!theta0, !g') = randomTheta nParams g
            (!thetaOpt, _, _) = minimizeNLL MSE Nothing nIter xTrain yTrain tree theta0
            !yPred = M.compute (evalTree xTrain thetaOpt tree)
            !mse = mseScore yTrain yPred
            !fit = if isNaN mse || isInfinite mse then -1e18 else negate mse
        in if fit > bestFit
           then go (n-1) g' fit thetaOpt
           else go (n-1) g' bestFit bestTheta

-- | Generate random initial parameter vector
randomTheta :: Int -> StdGen -> (PVector, StdGen)
randomTheta n g = go n g []
  where
    go :: Int -> StdGen -> [Double] -> (PVector, StdGen)
    go 0 !g' !acc = (M.fromList @M.S M.Seq (reverse acc), g')
    go !k !g' !acc =
        let (!v, !g'') = randomR (-1.0 :: Double, 1.0) g'
        in go (k-1) g'' (v : acc)

-- | Evaluate an expression on a feature matrix with given parameters.
evaluateExpr :: SR.Fix SRTree -> PVector -> SRMatrix -> PVector
evaluateExpr tree theta xMatrix = M.compute (evalTree xMatrix theta tree)

-- | Compute R² (coefficient of determination).
r2Score :: PVector -> PVector -> Double
r2Score yTrue yPred =
    let !n = M.elemsCount yTrue
        !yMean = M.sum yTrue / fromIntegral n
        !ssTot = M.sum $ M.map (\y -> (y - yMean) ^ (2 :: Int)) yTrue
        !ssRes = M.sum $ M.zipWith (\yt yp -> (yt - yp) ^ (2 :: Int)) yTrue yPred
    in if ssTot == 0 then 0 else 1 - ssRes / ssTot

-- | Compute MSE (mean squared error).
mseScore :: PVector -> PVector -> Double
mseScore yTrue yPred =
    let !n = M.elemsCount yTrue
        !ssRes = M.sum $ M.zipWith (\yt yp -> (yt - yp) ^ (2 :: Int)) yTrue yPred
    in ssRes / fromIntegral n
