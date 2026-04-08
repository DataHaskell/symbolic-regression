{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parameter optimisation via reverse-mode AD on expression trees.

Uses NLOPT's Truncated Newton method (TNEWTON) for fast convergence.
Gradient computed via reverse-mode AD with cached forward tape.
-}
module Symbolic.Regression.Expr.Opt (
    Distribution (..),
    mse,
    nll,
    r2,
    minimizeNLL,
    minimizeNLLRidge,
    fractionalBayesFactor,
) where

import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalBinOp, evalTree, evalUnOp)
import Symbolic.Regression.Expr.NLOPT (minimizeTNEWTON)
import Symbolic.Regression.Expr.Utils (countNodes, countUniqueTokens)

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (foldl1')
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

------------------------------------------------------------------------
-- Distribution
------------------------------------------------------------------------

data Distribution = MSE | Gaussian | Bernoulli | Poisson
    deriving (Show, Read, Eq, Enum, Bounded)

------------------------------------------------------------------------
-- Loss (sum . zipWith fuses — no intermediate vector)
------------------------------------------------------------------------

mse :: PVector -> PVector -> Double
mse yt yp =
    VU.sum (VU.zipWith (\a b -> let d = a - b in d * d) yt yp)
        / fromIntegral (VU.length yt)
{-# INLINE mse #-}

nll ::
    Distribution ->
    Maybe PVector ->
    Features ->
    PVector ->
    Fix ExprF ->
    PVector ->
    Double
nll dist _mYerr xss ys tree theta =
    let yh = evalTree xss theta tree
     in case dist of
            MSE -> mse ys yh
            Gaussian ->
                let !n = fromIntegral (VU.length ys)
                    !ss = VU.sum (VU.zipWith (\y h -> let d = y - h in d * d) ys yh)
                    !s2 = ss / n
                 in n / 2 * log (2 * pi * s2) + ss / (2 * s2)
            Bernoulli ->
                negate
                    ( VU.sum
                        ( VU.zipWith
                            ( \y h ->
                                let p = max 1e-15 (min (1 - 1e-15) (1 / (1 + exp (-h))))
                                 in y * log p + (1 - y) * log (1 - p)
                            )
                            ys
                            yh
                        )
                    )
                    / fromIntegral (VU.length ys)
            Poisson ->
                negate
                    ( VU.sum
                        ( VU.zipWith
                            ( \y h ->
                                let mu = max 1e-15 (exp h) in y * log mu - mu
                            )
                            ys
                            yh
                        )
                    )
                    / fromIntegral (VU.length ys)

r2 :: Features -> PVector -> Fix ExprF -> PVector -> Double
r2 xss ys tree theta =
    let yh = evalTree xss theta tree
        !n = VU.length ys
        !nf = fromIntegral n
        !mu = VU.sum ys / nf
        !st = VU.sum (VU.map (\y -> let d = y - mu in d * d) ys)
        !sr = VU.sum (VU.zipWith (\y h -> let d = y - h in d * d) ys yh)
     in if st == 0 then 0 else 1 - sr / st

------------------------------------------------------------------------
-- Tape: forward pass caches output at every node for backward reuse
------------------------------------------------------------------------

-- | Tree annotated with cached output at every node (O(nodes) forward pass).
data Tape
    = TpLeaf !PVector -- out
    | TpParam {-# UNPACK #-} !Int !PVector -- param_ix, out
    | TpUn !UnOp !PVector !PVector !Tape -- op, child_val, out, child
    | TpBin !BinOp !PVector !PVector !PVector !Tape !Tape -- op, l_val, r_val, out, l, r
    | TpSum [PVector] !PVector [Tape] -- child_vals, out, children
    | TpProd [PVector] !PVector [Tape] -- child_vals, out, children

tpOut :: Tape -> PVector
tpOut (TpLeaf v) = v
tpOut (TpParam _ v) = v
tpOut (TpUn _ _ v _) = v
tpOut (TpBin _ _ _ v _ _) = v
tpOut (TpSum _ v _) = v
tpOut (TpProd _ v _) = v
{-# INLINE tpOut #-}

{- | Forward pass: evaluate tree, cache value at every node.
O(nodes) vector allocations — each used in backward pass.
-}
fwdTape :: Features -> PVector -> Fix ExprF -> Tape
fwdTape xss theta = go
  where
    !nR = V.length xss
    go (Fix node) = case node of
        VarF i -> TpLeaf $! VU.generate nR (\r -> xss `V.unsafeIndex` r `VU.unsafeIndex` i)
        LitF x -> TpLeaf $! VU.replicate nR x
        ParamF i -> TpParam i $! VU.replicate nR (theta `VU.unsafeIndex` i)
        UnF f c ->
            let !tc = go c
                !cv = tpOut tc
                !ov = VU.map (evalUnOp f) cv
             in TpUn f cv ov tc
        BinF op l r ->
            let !tl = go l
                !tr = go r
                !lv = tpOut tl
                !rv = tpOut tr
                !ov = VU.zipWith (evalBinOp op) lv rv
             in TpBin op lv rv ov tl tr
        SumF xs ->
            let tapes = map go xs
                vals = map tpOut tapes
                out = case vals of
                    [] -> VU.replicate nR 0
                    _ -> foldl1' (VU.zipWith (+)) vals
             in TpSum vals out tapes
        ProdF xs ->
            let tapes = map go xs
                vals = map tpOut tapes
                out = case vals of
                    [] -> VU.replicate nR 1
                    _ -> foldl1' (VU.zipWith (*)) vals
             in TpProd vals out tapes
        PolyF _ -> error "fwdTape: PolyF not supported"
{-# INLINE fwdTape #-}

{- | Backward pass on tape: propagate adjoint, accumulate into mutable grad.
Each node reads its cached child values (no re-evaluation).
Adjoint * derivative fused into single zipWith per node.
-}
bwdTape :: Tape -> PVector -> VUM.MVector s Double -> ST s ()
bwdTape tape adj grad = case tape of
    TpLeaf _ -> pure ()
    TpParam i _ -> VUM.unsafeModify grad (+ VU.sum adj) i
    TpUn f cv _ tc ->
        let !cAdj = case f of
                Neg -> VU.map negate adj
                Abs -> VU.zipWith (\a c -> a * signum c) adj cv
                Recip -> VU.zipWith (\a c -> if c == 0 then 0 else -(a / (c * c))) adj cv
                Sq -> VU.zipWith (\a c -> a * 2 * c) adj cv
                Cube -> VU.zipWith (\a c -> a * 3 * c * c) adj cv
                Sqrt ->
                    VU.zipWith
                        (\a c -> let s = sqrt (abs c) in if s == 0 then 0 else a * 0.5 / s)
                        adj
                        cv
                Exp -> VU.zipWith (\a c -> a * exp (min 700 c)) adj cv
                Log -> VU.zipWith (\a c -> if c <= 0 then a / (abs c + 1e-300) else a / c) adj cv
                Sin -> VU.zipWith (\a c -> a * cos c) adj cv
                Cos -> VU.zipWith (\a c -> negate (a * sin c)) adj cv
         in bwdTape tc cAdj grad
    TpBin op lv rv _ tl tr -> case op of
        Add -> do bwdTape tl adj grad; bwdTape tr adj grad
        Sub -> do bwdTape tl adj grad; bwdTape tr (VU.map negate adj) grad
        Mul -> do
            bwdTape tl (VU.zipWith (*) adj rv) grad
            bwdTape tr (VU.zipWith (*) adj lv) grad
        Div -> do
            bwdTape tl (VU.zipWith (\a r -> if r == 0 then a else a / r) adj rv) grad
            bwdTape
                tr
                (VU.zipWith3 (\a l r -> if r == 0 then 0 else -(a * l / (r * r))) adj lv rv)
                grad
        Pow -> do
            bwdTape tl (VU.zipWith3 (\a l r -> a * r * l ** (r - 1)) adj lv rv) grad
            bwdTape
                tr
                (VU.zipWith3 (\a l r -> if l <= 0 then 0 else a * l ** r * log l) adj lv rv)
                grad
    TpSum _ _ tapes ->
        forM_ tapes (\tc -> bwdTape tc adj grad)
    TpProd vals _ tapes ->
        case vals of
            [] -> pure ()
            [_] -> forM_ tapes (\tc -> bwdTape tc adj grad)
            _ -> do
                -- O(n) prefix/suffix product arrays instead of O(n²) per-child recomputation
                let vArr = V.fromList vals
                    ones = VU.replicate (VU.length adj) 1
                    prefix = V.scanl' (VU.zipWith (*)) ones vArr -- prefix[i] = prod(vals[0..i-1])
                    suffix = V.scanr' (VU.zipWith (*)) ones vArr -- suffix[i] = prod(vals[i+1..n-1])
                forM_ (zip tapes [0 ..]) $ \(tc, i) -> do
                    let !childAdj =
                            VU.zipWith
                                (*)
                                adj
                                ( VU.zipWith
                                    (*)
                                    (prefix `V.unsafeIndex` i)
                                    (suffix `V.unsafeIndex` (i + 1))
                                )
                    bwdTape tc childAdj grad
{-# INLINE bwdTape #-}

------------------------------------------------------------------------
-- NLOPT Truncated Newton optimizer
------------------------------------------------------------------------

{- | Optimise parameters via NLOPT's Truncated Newton method with reverse-mode AD.
Convergence is quadratic — reaches large constants (e.g. 45) in ~5 iterations
vs Adam's ~200.
-}
minimizeNLL ::
    Distribution ->
    Maybe PVector ->
    Int ->
    Features ->
    PVector ->
    Fix ExprF ->
    PVector ->
    (PVector, Double, Int)
minimizeNLL _dist _mYerr maxIter xss ys tree theta0
    | maxIter == 0 || VU.null theta0 =
        (theta0, mse ys (evalTree xss theta0 tree), 0)
    | otherwise =
        let !nP = VU.length theta0
            !nRf = fromIntegral (V.length xss) :: Double

            -- Objective + gradient: forward tape then backward AD
            objGrad :: VS.Vector Double -> (Double, VS.Vector Double)
            objGrad thetaS =
                let !theta = vuFromStorable thetaS
                    !tape = fwdTape xss theta tree
                    !yHat = tpOut tape
                    !loss = VU.sum (VU.zipWith (\h y -> let d = h - y in d * d) yHat ys) / nRf
                    !rootAdj = VU.zipWith (\h y -> 2 * (h - y) / nRf) yHat ys
                    !grad = runST $ do
                        g <- VUM.replicate nP (0 :: Double)
                        bwdTape tape rootAdj g
                        VU.freeze g
                 in (loss, vsFromUnboxed grad)

            !t0s = vsFromUnboxed theta0
            (!tOptS, !cost, !nEvals) = minimizeTNEWTON maxIter nP objGrad t0s
            !tOpt = vuFromStorable tOptS
            -- Use returned cost if valid, otherwise recompute
            !finalCost =
                if isNaN cost || isInfinite cost
                    then mse ys (evalTree xss tOpt tree)
                    else cost
         in (tOpt, finalCost, nEvals)

{- | Like 'minimizeNLL' but with an L2 (ridge) penalty on the parameters.

The objective becomes @MSE + rho * sum(theta_i^2)@ which remains smooth,
so TNEWTON handles it without issue.  Used by the boosting module for
periodic joint coefficient refit.
-}
minimizeNLLRidge ::
    Double ->
    Int ->
    Features ->
    PVector ->
    Fix ExprF ->
    PVector ->
    (PVector, Double, Int)
minimizeNLLRidge rho maxIter xss ys tree theta0
    | maxIter == 0 || VU.null theta0 =
        (theta0, mse ys (evalTree xss theta0 tree), 0)
    | otherwise =
        let !nP = VU.length theta0
            !nRf = fromIntegral (V.length xss) :: Double

            objGrad :: VS.Vector Double -> (Double, VS.Vector Double)
            objGrad thetaS =
                let !theta = vuFromStorable thetaS
                    !tape = fwdTape xss theta tree
                    !yHat = tpOut tape
                    !mseLoss = VU.sum (VU.zipWith (\h y -> let d = h - y in d * d) yHat ys) / nRf
                    !ridgeLoss = rho * VU.sum (VU.map (\t -> t * t) theta)
                    !loss = mseLoss + ridgeLoss
                    !rootAdj = VU.zipWith (\h y -> 2 * (h - y) / nRf) yHat ys
                    !mseGrad = runST $ do
                        g <- VUM.replicate nP (0 :: Double)
                        bwdTape tape rootAdj g
                        VU.freeze g
                    !grad = VU.zipWith (\gi ti -> gi + 2 * rho * ti) mseGrad theta
                 in (loss, vsFromUnboxed grad)

            !t0s = vsFromUnboxed theta0
            (!tOptS, !cost, !nEvals) = minimizeTNEWTON maxIter nP objGrad t0s
            !tOpt = vuFromStorable tOptS
            !finalCost =
                if isNaN cost || isInfinite cost
                    then mse ys (evalTree xss tOpt tree)
                    else cost
         in (tOpt, finalCost, nEvals)

-- | Convert Storable vector to Unboxed vector
vuFromStorable :: VS.Vector Double -> VU.Vector Double
vuFromStorable sv = VU.generate (VS.length sv) (VS.unsafeIndex sv)
{-# INLINE vuFromStorable #-}

-- | Convert Unboxed vector to Storable vector
vsFromUnboxed :: VU.Vector Double -> VS.Vector Double
vsFromUnboxed uv = VS.generate (VU.length uv) (VU.unsafeIndex uv)
{-# INLINE vsFromUnboxed #-}

------------------------------------------------------------------------
-- Model selection
------------------------------------------------------------------------

fractionalBayesFactor ::
    Distribution ->
    Maybe PVector ->
    Features ->
    PVector ->
    PVector ->
    Fix ExprF ->
    Double
fractionalBayesFactor dist mYerr xss ys theta tree =
    (1 - b) * nll' - p / 2 * log b + fc + p / 2 * log (2 * pi * nup)
  where
    nll' =
        if dist == MSE
            then nll Gaussian mYerr xss ys tree theta
            else nll dist mYerr xss ys tree theta
    !p = fromIntegral (VU.length theta)
    !n = fromIntegral (VU.length ys)
    !b = 1 / sqrt n
    !nup = exp (1 - log 3)
    !fc = fromIntegral (countNodes tree) * log (fromIntegral (countUniqueTokens tree))
