{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | hegg 'Language' and 'Analysis' instances for 'ExprF'.

'normalizeNode' sorts children of commutative operators (Add, Mul),
providing A\/C canonicalization without rewrite rules.

The analysis domain 'SRAnalysis' tracks constant folding, expression size,
and optionally fitness\/parameter metadata per e-class (shared across
equivalent expressions, per insights.md).
-}
module Symbolic.Regression.Language (
    SRAnalysis (..),
    SRConst (..),
    srCost,
    exprNormalize,
    exprNormalizeBinary,
) where

import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import qualified Data.Set as S

import Data.Equality.Analysis
import Data.Equality.Extraction
import Data.Equality.Graph
import Data.Equality.Graph.Internal (EGraph (..))
import Data.Equality.Graph.Lens

import Symbolic.Regression.Expr

{- | 'Language' instance for 'ExprF'.
Encodes the commutative semiring structure:
- Commutativity: sort children of Add/Mul
- Sub elimination: a - b → a + neg(b)
- Div elimination: a / b → a * recip(b)
- AC flattening: Add(Add(a,b),c) → right-associated sorted form
-}
instance Language ExprF where
    normalizeNode (BinF Add a b) = BinF Add (min a b) (max a b)
    normalizeNode (BinF Mul a b) = BinF Mul (min a b) (max a b)
    normalizeNode (SumF xs) = SumF (sort xs)
    normalizeNode (ProdF xs) = ProdF (sort xs)
    normalizeNode other = other

{- | Context-aware normalization for ExprF. Encodes the commutative semiring:
Sub → Add + Neg, Div → Mul + Recip, AC flattening for Add/Mul.
Used with 'addWithNorm' from hegg.
-}
exprNormalize ::
    (Analysis a ExprF) =>
    ExprF ClassId -> EGraph a ExprF -> (ExprF ClassId, EGraph a ExprF)
exprNormalize node eg = case node of
    -- Sub a b → Add a (Neg b)
    BinF Sub a b ->
        let (negB, eg1) = add (Node (UnF Neg b)) eg
         in (BinF Add (min a negB) (max a negB), eg1)
    -- Div a b → Mul a (Recip b)
    BinF Div a b ->
        let (recipB, eg1) = add (Node (UnF Recip b)) eg
         in (BinF Mul (min a recipB) (max a recipB), eg1)
    -- Flatten and sort Add chains into SumF
    BinF Add a b ->
        let aLeaves = collectSumLeaves a eg
            bLeaves = collectSumLeaves b eg
            sorted = sort (aLeaves ++ bLeaves)
         in (SumF sorted, eg)
    -- Flatten and sort Mul chains into ProdF
    BinF Mul a b ->
        let aLeaves = collectProdLeaves a eg
            bLeaves = collectProdLeaves b eg
            sorted = sort (aLeaves ++ bLeaves)
         in (ProdF sorted, eg)
    -- Re-flatten SumF
    SumF xs -> (SumF (sort (concatMap (`collectSumLeaves` eg) xs)), eg)
    -- Re-flatten ProdF
    ProdF xs -> (ProdF (sort (concatMap (`collectProdLeaves` eg) xs)), eg)
    other -> (normalizeNode other, eg)

-- | Collect sum leaves: if the e-class contains a SumF, return its children; otherwise [cid].
collectSumLeaves :: ClassId -> EGraph a ExprF -> [ClassId]
collectSumLeaves cid eg =
    let cid' = find cid eg
     in case IM.lookup cid' (classes eg) of
            Nothing -> [cid']
            Just ec ->
                case S.foldl'
                    ( \acc (Node n) -> case (acc, n) of
                        (Nothing, SumF xs) -> Just xs
                        _ -> acc
                    )
                    Nothing
                    (eClassNodes ec) of
                    Just xs -> concatMap (`collectSumLeaves` eg) xs
                    Nothing -> [cid']

-- | Collect product leaves: if the e-class contains a ProdF, return its children; otherwise [cid].
collectProdLeaves :: ClassId -> EGraph a ExprF -> [ClassId]
collectProdLeaves cid eg =
    let cid' = find cid eg
     in case IM.lookup cid' (classes eg) of
            Nothing -> [cid']
            Just ec ->
                case S.foldl'
                    ( \acc (Node n) -> case (acc, n) of
                        (Nothing, ProdF xs) -> Just xs
                        _ -> acc
                    )
                    Nothing
                    (eClassNodes ec) of
                    Just xs -> concatMap (`collectProdLeaves` eg) xs
                    Nothing -> [cid']

{- | Binary-only normalization (no SumF/ProdF containers).
Sub → Add + Neg, Div → Mul + Recip, commutativity sort only.
-}
exprNormalizeBinary ::
    (Analysis a ExprF) =>
    ExprF ClassId -> EGraph a ExprF -> (ExprF ClassId, EGraph a ExprF)
exprNormalizeBinary node eg = case node of
    BinF Sub a b ->
        let (negB, eg1) = add (Node (UnF Neg b)) eg
         in (BinF Add (min a negB) (max a negB), eg1)
    BinF Div a b ->
        let (recipB, eg1) = add (Node (UnF Recip b)) eg
         in (BinF Mul (min a recipB) (max a recipB), eg1)
    other -> (normalizeNode other, eg)

-- | Constant tracking for e-class analysis.
data SRConst
    = NotConst
    | IsParam {-# UNPACK #-} !Int
    | IsConst {-# UNPACK #-} !Double
    deriving (Show)

instance Eq SRConst where
    NotConst == NotConst = True
    IsParam i == IsParam j = i == j
    IsConst x == IsConst y = x == y
    _ == _ = False

-- | Analysis domain: constant folding, expression size, and class-level fitness.
data SRAnalysis = SRAnalysis
    { srConst :: !SRConst
    , srSize :: {-# UNPACK #-} !Int
    , srFitness :: !(Maybe Double)
    , srTheta :: !(Maybe [PVector])
    }
    deriving (Eq, Show)

evalBinConst :: BinOp -> SRConst -> SRConst -> SRConst
evalBinConst op (IsConst x) (IsConst y) = case op of
    Add -> IsConst (x + y)
    Sub -> IsConst (x - y)
    Mul -> IsConst (x * y)
    Div
        | y /= 0 -> IsConst (x / y)
        | otherwise -> NotConst
    Pow -> IsConst (x ** y)
evalBinConst Mul (IsConst 0) _ = IsConst 0
evalBinConst Mul _ (IsConst 0) = IsConst 0
evalBinConst Add (IsConst 0) c = c
evalBinConst Add c (IsConst 0) = c
evalBinConst Mul (IsConst 1) c = c
evalBinConst Mul c (IsConst 1) = c
evalBinConst _ _ _ = NotConst

evalUnConst :: UnOp -> SRConst -> SRConst
evalUnConst f (IsConst x) = case f of
    Exp -> IsConst (exp x)
    Log | x > 0 -> IsConst (log x)
    Sqrt | x >= 0 -> IsConst (sqrt x)
    Sq -> IsConst (x * x)
    Cube -> IsConst (x * x * x)
    Recip | x /= 0 -> IsConst (1 / x)
    Abs -> IsConst (abs x)
    Sin -> IsConst (sin x)
    Cos -> IsConst (cos x)
    Neg -> IsConst (negate x)
    _ -> NotConst
evalUnConst _ _ = NotConst

joinConst :: SRConst -> SRConst -> SRConst
joinConst (IsConst x) (IsConst y)
    | x == y = IsConst x
    | otherwise = NotConst
joinConst NotConst c = c
joinConst c NotConst = c
joinConst _ _ = NotConst

instance Analysis SRAnalysis ExprF where
    makeA = \case
        LitF x -> SRAnalysis (IsConst x) 1 Nothing Nothing
        ParamF i -> SRAnalysis (IsParam i) 1 Nothing Nothing
        VarF _ -> SRAnalysis NotConst 1 Nothing Nothing
        BinF op a b ->
            SRAnalysis
                (evalBinConst op (srConst a) (srConst b))
                (srSize a + srSize b + 1)
                Nothing
                Nothing
        UnF f a ->
            SRAnalysis
                (evalUnConst f (srConst a))
                (srSize a + 1)
                Nothing
                Nothing
        SumF xs ->
            SRAnalysis
                (foldl' (\acc x -> evalBinConst Add acc (srConst x)) (IsConst 0) xs)
                (1 + sum (map srSize xs))
                Nothing
                Nothing
        ProdF xs ->
            SRAnalysis
                (foldl' (\acc x -> evalBinConst Mul acc (srConst x)) (IsConst 1) xs)
                (1 + sum (map srSize xs))
                Nothing
                Nothing

    joinA a b =
        SRAnalysis
            (joinConst (srConst a) (srConst b))
            (min (srSize a) (srSize b))
            (joinFitness (srFitness a) (srTheta a) (srFitness b) (srTheta b))
            (joinTheta (srFitness a) (srTheta a) (srFitness b) (srTheta b))

    -- Only constant folding in modifyA. Sub/Div normalization and AC
    -- flattening are handled structurally by normalizeInContext.
    modifyA cid eg =
        case srConst (eg ^. _class cid . _data) of
            IsConst x ->
                let (cid', eg') = represent (Fix (LitF x)) eg
                 in snd $ merge cid cid' eg'
            _ -> eg

-- | Keep the better fitness when merging.
joinFitness ::
    Maybe Double ->
    Maybe [PVector] ->
    Maybe Double ->
    Maybe [PVector] ->
    Maybe Double
joinFitness (Just a) _ (Just b) _ = Just (max a b)
joinFitness a _ Nothing _ = a
joinFitness Nothing _ b _ = b

joinTheta ::
    Maybe Double ->
    Maybe [PVector] ->
    Maybe Double ->
    Maybe [PVector] ->
    Maybe [PVector]
joinTheta (Just a) ta (Just b) tb = if a >= b then ta else tb
joinTheta _ ta Nothing _ = ta
joinTheta Nothing _ _ tb = tb

-- | Cost function: smaller is better.
srCost :: CostFunction SRAnalysis ExprF Int
srCost _anl = \case
    VarF _ -> 1
    LitF _ -> 1
    ParamF _ -> 1
    BinF _ (_, c1) (_, c2) -> 2 + c1 + c2
    UnF _ (_, c) -> 3 + c
    SumF xs -> 1 + sum (map snd xs)
    ProdF xs -> 1 + sum (map snd xs)
