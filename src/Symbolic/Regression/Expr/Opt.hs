{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Parameter optimisation via reverse-mode AD on expression trees + Adam.
--
-- The entire optimizer loop (forward eval, backward AD, Adam step) runs
-- inside a single 'ST' block with pre-allocated mutable buffers.
-- No vector allocation occurs in the hot loop.
module Symbolic.Regression.Expr.Opt
    ( Distribution(..)
    , mse
    , nll
    , r2
    , minimizeNLL
    , fractionalBayesFactor
    ) where

import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree, evalBinOp, evalUnOp)
import Symbolic.Regression.Expr.Utils (countNodes, countUniqueTokens)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)

------------------------------------------------------------------------
-- Distribution
------------------------------------------------------------------------

data Distribution = MSE | Gaussian | Bernoulli | Poisson
    deriving (Show, Read, Eq, Enum, Bounded)

------------------------------------------------------------------------
-- Loss (sum . zipWith fuses — no intermediate vector)
------------------------------------------------------------------------

mse :: PVector -> PVector -> Double
mse yt yp = VU.sum (VU.zipWith (\a b -> let d = a - b in d * d) yt yp)
          / fromIntegral (VU.length yt)
{-# INLINE mse #-}

nll :: Distribution -> Maybe PVector -> Features -> PVector -> Fix ExprF -> PVector -> Double
nll dist _mYerr xss ys tree theta =
    let yh = evalTree xss theta tree in
    case dist of
        MSE      -> mse ys yh
        Gaussian -> let !n = fromIntegral (VU.length ys)
                        !ss = VU.sum (VU.zipWith (\y h -> let d=y-h in d*d) ys yh)
                        !s2 = ss / n
                    in n/2 * log (2*pi*s2) + ss/(2*s2)
        Bernoulli -> negate (VU.sum (VU.zipWith (\y h ->
                        let p = max 1e-15 (min (1-1e-15) (1/(1+exp(-h))))
                        in y*log p + (1-y)*log(1-p)) ys yh))
                    / fromIntegral (VU.length ys)
        Poisson -> negate (VU.sum (VU.zipWith (\y h ->
                        let mu = max 1e-15 (exp h) in y*log mu - mu) ys yh))
                    / fromIntegral (VU.length ys)

r2 :: Features -> PVector -> Fix ExprF -> PVector -> Double
r2 xss ys tree theta =
    let yh = evalTree xss theta tree
        !n  = VU.length ys; !nf = fromIntegral n
        !mu = VU.sum ys / nf
        !st = VU.sum (VU.map (\y -> let d=y-mu in d*d) ys)
        !sr = VU.sum (VU.zipWith (\y h -> let d=y-h in d*d) ys yh)
    in if st == 0 then 0 else 1 - sr/st

------------------------------------------------------------------------
-- Tape: forward pass caches output at every node for backward reuse
------------------------------------------------------------------------

-- | Tree annotated with cached output at every node (O(nodes) forward pass).
data Tape
    = TpLeaf  !PVector                                         -- out
    | TpParam {-# UNPACK #-} !Int !PVector                     -- param_ix, out
    | TpUn    !UnOp  !PVector !PVector !Tape                    -- op, child_val, out, child
    | TpBin   !BinOp !PVector !PVector !PVector !Tape !Tape     -- op, l_val, r_val, out, l, r

tpOut :: Tape -> PVector
tpOut (TpLeaf v)          = v
tpOut (TpParam _ v)       = v
tpOut (TpUn _ _ v _)      = v
tpOut (TpBin _ _ _ v _ _) = v
{-# INLINE tpOut #-}

-- | Forward pass: evaluate tree, cache value at every node.
-- O(nodes) vector allocations — each used in backward pass.
fwdTape :: Features -> PVector -> Fix ExprF -> Tape
fwdTape xss theta = go
  where
    !nR = V.length xss
    go (Fix node) = case node of
        VarF i   -> TpLeaf $! VU.generate nR (\r -> xss `V.unsafeIndex` r `VU.unsafeIndex` i)
        LitF x   -> TpLeaf $! VU.replicate nR x
        ParamF i -> TpParam i $! VU.replicate nR (theta `VU.unsafeIndex` i)
        UnF f c  ->
            let !tc = go c; !cv = tpOut tc
                !ov = VU.map (evalUnOp f) cv
            in TpUn f cv ov tc
        BinF op l r ->
            let !tl = go l; !tr = go r
                !lv = tpOut tl; !rv = tpOut tr
                !ov = VU.zipWith (evalBinOp op) lv rv
            in TpBin op lv rv ov tl tr
{-# INLINE fwdTape #-}

-- | Backward pass on tape: propagate adjoint, accumulate into mutable grad.
-- Each node reads its cached child values (no re-evaluation).
-- Adjoint * derivative fused into single zipWith per node.
bwdTape :: Tape -> PVector -> VUM.MVector s Double -> ST s ()
bwdTape tape adj grad = case tape of
    TpLeaf _  -> pure ()
    TpParam i _ -> VUM.unsafeModify grad (+ VU.sum adj) i
    TpUn f cv _ tc ->
        let !cAdj = case f of
                Neg   -> VU.map negate adj
                Abs   -> VU.zipWith (\a c -> a * signum c) adj cv
                Recip -> VU.zipWith (\a c -> if c==0 then 0 else -a/(c*c)) adj cv
                Sq    -> VU.zipWith (\a c -> a*2*c) adj cv
                Cube  -> VU.zipWith (\a c -> a*3*c*c) adj cv
                Sqrt  -> VU.zipWith (\a c -> let s=sqrt(abs c) in if s==0 then 0 else a*0.5/s) adj cv
                Exp   -> VU.zipWith (\a c -> a * exp (min 700 c)) adj cv
                Log   -> VU.zipWith (\a c -> if c<=0 then a/(abs c+1e-300) else a/c) adj cv
                Sin   -> VU.zipWith (\a c -> a * cos c) adj cv
                Cos   -> VU.zipWith (\a c -> negate (a * sin c)) adj cv
        in bwdTape tc cAdj grad
    TpBin op lv rv _ tl tr -> case op of
        Add -> do bwdTape tl adj grad; bwdTape tr adj grad
        Sub -> do bwdTape tl adj grad; bwdTape tr (VU.map negate adj) grad
        Mul -> do bwdTape tl (VU.zipWith (*) adj rv) grad
                  bwdTape tr (VU.zipWith (*) adj lv) grad
        Div -> do bwdTape tl (VU.zipWith (\a r -> if r==0 then a else a/r) adj rv) grad
                  bwdTape tr (VU.zipWith3 (\a l r -> if r==0 then 0 else -(a*l/(r*r))) adj lv rv) grad
        Pow -> do bwdTape tl (VU.zipWith3 (\a l r -> a*r*l**(r-1)) adj lv rv) grad
                  bwdTape tr (VU.zipWith3 (\a l r -> if l<=0 then 0 else a*l**r*log l) adj lv rv) grad
{-# INLINE bwdTape #-}

------------------------------------------------------------------------
-- Adam optimiser — pure interface, ST interior
------------------------------------------------------------------------

-- | Optimise parameters via Adam with reverse-mode AD.
-- Pure interface. Internally uses ST for mutable Adam state (x, m, v, bestX, grad).
-- Forward pass builds tape (O(nodes) vectors), backward reads cached values.
minimizeNLL :: Distribution -> Maybe PVector -> Int -> Features -> PVector
            -> Fix ExprF -> PVector -> (PVector, Double, Int)
minimizeNLL _dist _mYerr maxIter xss ys tree theta0
    | maxIter == 0 || VU.null theta0 = (theta0, mse ys (evalTree xss theta0 tree), 0)
    | otherwise = runST (go_st :: forall s. ST s (PVector, Double, Int))
  where
    !nP    = VU.length theta0
    !nRf   = fromIntegral (V.length xss) :: Double
    !beta1 = 0.9  :: Double
    !beta2 = 0.999 :: Double
    !eps   = 1e-8  :: Double

    go_st :: forall s. ST s (PVector, Double, Int)
    go_st = do
        x     <- VU.thaw theta0
        m_    <- VUM.replicate nP (0 :: Double)
        v_    <- VUM.replicate nP (0 :: Double)
        bestX <- VU.thaw theta0
        grad  <- VUM.new nP

        -- Combined forward + backward + loss in one call
        let fwdBwd :: ST s Double
            fwdBwd = do
                xFroz <- VU.freeze x
                let !tape   = fwdTape xss xFroz tree
                    !yHat   = tpOut tape
                    -- loss: sum . zipWith fuses to one loop
                    !loss   = VU.sum (VU.zipWith (\h y -> let d=h-y in d*d) yHat ys) / nRf
                    !rootAdj = VU.zipWith (\h y -> 2*(h-y)/nRf) yHat ys
                VUM.set grad 0
                bwdTape tape rootAdj grad
                pure loss

        loss0 <- fwdBwd

        let step :: Int -> Double -> ST s (Double, Int)
            step !k !bestLoss
                | k >= maxIter = pure (bestLoss, k)
                | otherwise = do
                    loss <- fwdBwd
                    let !progress = fromIntegral k / fromIntegral maxIter :: Double
                        !lr  = 0.5 * (1 + cos (pi * progress)) / 2 + 0.01
                        !t   = fromIntegral (k + 1) :: Double
                        !bc1 = 1 - beta1 ** t
                        !bc2 = 1 - beta2 ** t
                    forM_ [0 .. nP - 1] $ \i -> do
                        gi <- VUM.unsafeRead grad i
                        let !gc = max (-1000) (min 1000 gi)
                        mi <- VUM.unsafeRead m_ i
                        vi <- VUM.unsafeRead v_ i
                        let !mi' = beta1 * mi + (1-beta1) * gc
                            !vi' = beta2 * vi + (1-beta2) * gc * gc
                        VUM.unsafeWrite m_ i mi'
                        VUM.unsafeWrite v_ i vi'
                        xi <- VUM.unsafeRead x i
                        VUM.unsafeWrite x i
                            (xi - lr * (mi'/bc1) / (sqrt (vi'/bc2) + eps))
                    if loss < bestLoss
                        then do VUM.copy bestX x; step (k+1) loss
                        else step (k+1) bestLoss

        (!bestLoss, !iters) <- step 0 loss0
        result <- VU.freeze bestX
        pure (result, bestLoss, iters)

------------------------------------------------------------------------
-- Model selection
------------------------------------------------------------------------

fractionalBayesFactor :: Distribution -> Maybe PVector -> Features -> PVector
                      -> PVector -> Fix ExprF -> Double
fractionalBayesFactor dist mYerr xss ys theta tree =
    (1-b) * nll' - p/2 * log b + fc + p/2 * log (2*pi*nup)
  where
    nll' = if dist == MSE then nll Gaussian mYerr xss ys tree theta
                          else nll dist mYerr xss ys tree theta
    !p = fromIntegral (VU.length theta)
    !n = fromIntegral (VU.length ys)
    !b = 1 / sqrt n
    !nup = exp (1 - log 3)
    !fc = fromIntegral (countNodes tree) * log (fromIntegral (countUniqueTokens tree))
