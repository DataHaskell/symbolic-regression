{-# LANGUAGE BangPatterns #-}

{- | Minimal NLOPT wrapper for parameter optimization.
Uses Truncated Newton (TNEWTON) with gradient via reverse-mode AD.
-}
module Symbolic.Regression.Expr.NLOPT (
    minimizeTNEWTON,
) where

import qualified Data.Vector.Storable as VS
import qualified Numeric.Optimization.NLOPT.Bindings as N
import System.IO.Unsafe (unsafePerformIO)

{- | Minimize an objective using TNEWTON (Truncated Newton's method).
The objective function returns (cost, gradient).
Returns (optimized params, final cost, number of evaluations).
-}
minimizeTNEWTON ::
    -- | Max evaluations
    Int ->
    -- | Number of parameters
    Int ->
    -- | Objective + gradient
    (VS.Vector Double -> (Double, VS.Vector Double)) ->
    -- | Initial parameters
    VS.Vector Double ->
    -- | (theta_opt, cost, nEvals)
    (VS.Vector Double, Double, Int)
minimizeTNEWTON maxEval nParams objGrad theta0 = unsafePerformIO $ do
    mOpt <- N.create N.LD_TNEWTON (fromIntegral nParams)
    case mOpt of
        Nothing -> pure (theta0, 1e18, 0)
        Just opt -> do
            -- Set objective: wraps our (cost, grad) function for the C callback
            _ <- N.set_min_objective opt scalarFun ()
            -- Stopping criteria
            _ <- N.set_ftol_rel opt 1e-6
            _ <- N.set_ftol_abs opt 1e-6
            _ <- N.set_maxeval opt (fromIntegral maxEval)
            _ <- N.set_vector_storage opt (fromIntegral nParams)
            -- Run
            result <- N.optimize opt theta0
            let ok = N.isSuccess (N.resultCode result)
            if ok
                then pure (N.resultParameters result, N.resultCost result, N.nEvals result)
                else pure (theta0, 1e18, 0)
  where
    scalarFun :: N.ScalarFunction ()
    scalarFun params mGrad _ = do
        let (!cost, !gradient) = objGrad params
        case mGrad of
            Nothing -> pure ()
            Just gVec -> VS.copy gVec gradient
        pure cost
{-# NOINLINE minimizeTNEWTON #-}
