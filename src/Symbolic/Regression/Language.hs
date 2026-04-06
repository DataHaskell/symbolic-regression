{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | hegg 'Language' and 'Analysis' instances for srtree's 'SRTree'.
--
-- The key feature is 'normalizeNode' which sorts children of commutative
-- operators (Add, Mul), providing A\/C canonicalization without rewrite rules.
module Symbolic.Regression.Language
    ( SRAnalysis(..)
    , SRConst(..)
    , srCost
    , toHeggFix
    , fromHeggFix
    ) where

import Data.Equality.Graph
import Data.Equality.Graph.Lens
import Data.Equality.Analysis
import Data.Equality.Extraction

import Data.SRTree.Internal (SRTree(..), Op(..), Function(..))
import qualified Data.SRTree.Recursion as SR
import qualified Data.Equality.Utils as H

-- | 'Language' instance for 'SRTree'.
-- 'normalizeNode' sorts children of commutative operators (Add, Mul),
-- eliminating the need for commutativity rewrite rules.
instance Language SRTree where
    normalizeNode (Bin Add a b) = Bin Add (min a b) (max a b)
    normalizeNode (Bin Mul a b) = Bin Mul (min a b) (max a b)
    normalizeNode other         = other

-- | Constant tracking for e-class analysis.
data SRConst
    = NotConst
    | IsParam  {-# UNPACK #-} !Int
    | IsConst  {-# UNPACK #-} !Double
    deriving (Show)

instance Eq SRConst where
    NotConst  == NotConst    = True
    IsParam i == IsParam j   = i == j
    IsConst x == IsConst y   = x == y
    _         == _           = False

-- | Analysis domain: constant folding + expression size.
data SRAnalysis = SRAnalysis
    { srConst :: !SRConst
    , srSize  :: {-# UNPACK #-} !Int
    } deriving (Eq, Show)

-- | Evaluate a binary op on two known constants.
evalBinConst :: Op -> SRConst -> SRConst -> SRConst
evalBinConst op (IsConst x) (IsConst y) = case op of
    Add -> IsConst (x + y)
    Sub -> IsConst (x - y)
    Mul -> IsConst (x * y)
    Div | y /= 0   -> IsConst (x / y)
        | otherwise -> NotConst
    Power -> IsConst (x ** y)
    _     -> NotConst
evalBinConst Mul (IsConst 0) _ = IsConst 0
evalBinConst Mul _ (IsConst 0) = IsConst 0
evalBinConst Add (IsConst 0) c = c
evalBinConst Add c (IsConst 0) = c
evalBinConst Mul (IsConst 1) c = c
evalBinConst Mul c (IsConst 1) = c
evalBinConst _ _ _ = NotConst

-- | Evaluate a unary function on a known constant.
evalUniConst :: Function -> SRConst -> SRConst
evalUniConst f (IsConst x) = case f of
    Exp    -> IsConst (exp x)
    Log    | x > 0 -> IsConst (log x)
    Sqrt   | x >= 0 -> IsConst (sqrt x)
    Square -> IsConst (x * x)
    Cube   -> IsConst (x * x * x)
    Recip  | x /= 0 -> IsConst (1 / x)
    Abs    -> IsConst (abs x)
    Sin    -> IsConst (sin x)
    Cos    -> IsConst (cos x)
    Id     -> IsConst x
    _      -> NotConst
evalUniConst _ _ = NotConst

-- | Join two constant analyses (when e-classes merge).
joinConst :: SRConst -> SRConst -> SRConst
joinConst (IsConst x) (IsConst y)
    | x == y    = IsConst x
    | otherwise = NotConst
joinConst NotConst c = c
joinConst c NotConst = c
joinConst _ _        = NotConst

instance Analysis SRAnalysis SRTree where
    makeA = \case
        Const x   -> SRAnalysis (IsConst x) 1
        Param i   -> SRAnalysis (IsParam i) 1
        Var _     -> SRAnalysis NotConst 1
        Bin op a b -> SRAnalysis
            (evalBinConst op (srConst a) (srConst b))
            (srSize a + srSize b + 1)
        Uni f a   -> SRAnalysis
            (evalUniConst f (srConst a))
            (srSize a + 1)

    joinA a b = SRAnalysis
        (joinConst (srConst a) (srConst b))
        (min (srSize a) (srSize b))

    -- When an e-class is determined to be a constant, add the Const node
    modifyA cid eg =
        case srConst (eg ^. _class cid . _data) of
            IsConst x ->
                let (cid', eg') = represent (H.Fix (Const x)) eg
                in snd $ merge cid cid' eg'
            _ -> eg

-- | Convert srtree's Fix to hegg's Fix (structurally identical newtypes)
toHeggFix :: SR.Fix SRTree -> H.Fix SRTree
toHeggFix (SR.Fix node) = H.Fix (fmap toHeggFix node)

-- | Convert hegg's Fix back to srtree's Fix
fromHeggFix :: H.Fix SRTree -> SR.Fix SRTree
fromHeggFix (H.Fix node) = SR.Fix (fmap fromHeggFix node)

-- | Cost function matching srtree's myCost.
-- Variables are cheapest, constants/params slightly more, operators most.
srCost :: CostFunction SRAnalysis SRTree Int
srCost _anl = \case
    Var _                  -> 1
    Const _                -> 1
    Param _                -> 1
    Bin _ (_, c1) (_, c2)  -> 2 + c1 + c2
    Uni _ (_, c)           -> 3 + c
