{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Pure tree utilities for the expression functor.
module Symbolic.Regression.Expr.Utils (
    countNodes,
    countParams,
    countParamsUniq,
    countUniqueTokens,
    relabelParams,
    relabelParamsOrder,
    paramsToConst,
    constsToParam,
    childrenOf,
    replaceChildren,
) where

import Symbolic.Regression.Expr

import Control.Monad.State.Strict (evalState, get, modify, put)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU

countNodes :: Fix ExprF -> Int
countNodes = cata $ \case
    VarF _ -> 1
    ParamF _ -> 1
    LitF _ -> 1
    UnF _ n -> 1 + n
    BinF _ l r -> 1 + l + r
    SumF xs -> 1 + sum xs
    ProdF xs -> 1 + sum xs
{-# INLINE countNodes #-}

countParams :: Fix ExprF -> Int
countParams = cata $ \case
    ParamF _ -> 1
    UnF _ n -> n
    BinF _ l r -> l + r
    SumF xs -> sum xs
    ProdF xs -> sum xs
    _ -> 0
{-# INLINE countParams #-}

countParamsUniq :: Fix ExprF -> Int
countParamsUniq = IS.size . cata alg
  where
    alg (ParamF i) = IS.singleton i
    alg (UnF _ s) = s
    alg (BinF _ l r) = IS.union l r
    alg (SumF ss) = IS.unions ss
    alg (ProdF ss) = IS.unions ss
    alg _ = IS.empty
{-# INLINE countParamsUniq #-}

countUniqueTokens :: Fix ExprF -> Int
countUniqueTokens = score . cata alg
  where
    score (ops, fns, vs, ps, cs) =
        S.size ops + S.size fns + S.size vs + S.size ps + S.size cs
    alg (VarF i) = (mempty, mempty, S.singleton i, mempty, mempty)
    alg (ParamF _) = (mempty, mempty, mempty, S.singleton (0 :: Int), mempty)
    alg (LitF _) = (mempty, mempty, mempty, mempty, S.singleton (0 :: Int))
    alg (UnF f t) = (mempty, S.singleton f, mempty, mempty, mempty) <> t
    alg (BinF op l r) = (S.singleton op, mempty, mempty, mempty, mempty) <> l <> r
    alg (SumF xs) = (S.singleton Add, mempty, mempty, mempty, mempty) <> mconcat xs
    alg (ProdF xs) = (S.singleton Mul, mempty, mempty, mempty, mempty) <> mconcat xs

------------------------------------------------------------------------
-- Parameter relabeling
------------------------------------------------------------------------

-- | Relabel parameters sequentially 0, 1, 2, ... in left-to-right order.
relabelParams :: Fix ExprF -> Fix ExprF
relabelParams t = evalState (go t) 0
  where
    go (Fix (ParamF _)) = do i <- get; modify (+ 1); pure (param i)
    go (Fix (UnF f c)) = Fix . UnF f <$> go c
    go (Fix (BinF op l r)) = do l' <- go l; r' <- go r; pure (Fix (BinF op l' r'))
    go (Fix (SumF xs)) = Fix . SumF <$> mapM go xs
    go (Fix (ProdF xs)) = Fix . ProdF <$> mapM go xs
    go other = pure other

-- | Relabel parameters by order of first occurrence (deduplicating).
relabelParamsOrder :: Fix ExprF -> Fix ExprF
relabelParamsOrder t = evalState (go t) (IM.empty, 0)
  where
    go (Fix (ParamF ix)) = do
        (m, next) <- get
        case IM.lookup ix m of
            Just j -> pure (param j)
            Nothing -> do put (IM.insert ix next m, next + 1); pure (param next)
    go (Fix (UnF f c)) = Fix . UnF f <$> go c
    go (Fix (BinF op l r)) = do l' <- go l; r' <- go r; pure (Fix (BinF op l' r'))
    go (Fix (SumF xs)) = Fix . SumF <$> mapM go xs
    go (Fix (ProdF xs)) = Fix . ProdF <$> mapM go xs
    go other = pure other

------------------------------------------------------------------------
-- Constant / parameter substitution
------------------------------------------------------------------------

-- | Substitute parameter values as literal constants.
paramsToConst :: [Double] -> Fix ExprF -> Fix ExprF
paramsToConst theta =
    let !tv = VU.fromList theta
     in cata $ \case
            ParamF i
                | i < VU.length tv -> lit (tv `VU.unsafeIndex` i)
                | otherwise -> Fix (ParamF i)
            VarF i -> var i
            LitF x -> lit x
            UnF f c -> Fix (UnF f c)
            BinF op l r -> Fix (BinF op l r)
            SumF xs -> Fix (SumF xs)
            ProdF xs -> Fix (ProdF xs)

-- | Convert all constants to parameters; return new tree and extracted values.
constsToParam :: Fix ExprF -> (Fix ExprF, [Double])
constsToParam t = let (t', vs) = cata alg t in (relabelParams t', vs)
  where
    alg (VarF i) = (var i, [])
    alg (ParamF _) = (param 0, [1.0])
    alg (LitF c) = (param 0, [c])
    alg (UnF f (c, v)) = (Fix (UnF f c), v)
    alg (BinF op (l, vl) (r, vr)) = (Fix (BinF op l r), vl <> vr)
    alg (SumF xs) = let (ts, vs) = unzip xs in (Fix (SumF ts), concat vs)
    alg (ProdF xs) = let (ts, vs) = unzip xs in (Fix (ProdF ts), concat vs)

------------------------------------------------------------------------
-- Node children
------------------------------------------------------------------------

-- | Extract the immediate children of an unfixed node.
childrenOf :: ExprF a -> [a]
childrenOf = \case
    UnF _ c -> [c]
    BinF _ l r -> [l, r]
    SumF xs -> xs
    ProdF xs -> xs
    _ -> []

-- | Replace children in a node template.
replaceChildren :: [a] -> ExprF b -> ExprF a
replaceChildren [l, r] (BinF op _ _) = BinF op l r
replaceChildren [c] (UnF f _) = UnF f c
replaceChildren cs (SumF _) = SumF cs
replaceChildren cs (ProdF _) = ProdF cs
replaceChildren _ (VarF i) = VarF i
replaceChildren _ (ParamF i) = ParamF i
replaceChildren _ (LitF x) = LitF x
replaceChildren _ _ = error "replaceChildren: arity mismatch"
