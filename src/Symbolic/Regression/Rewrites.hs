{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Algebraic rewrite rules for symbolic regression, ported from srtree's
-- Algorithm.EqSat.Simplify to hegg's rewrite API.
--
-- Commutativity rules are /omitted/ — they are handled structurally by
-- 'normalizeNode' in "Symbolic.Regression.Language".
module Symbolic.Regression.Rewrites
    ( srRewrites
    ) where

import Data.Equality.Graph
import Data.Equality.Graph.Lens
import Data.Equality.Matching
import Data.Equality.Matching.Database (Subst, findSubst)
import Data.Equality.Saturation.Rewrites

import Data.SRTree.Internal (SRTree(..), Op(..), Function(..))

import Symbolic.Regression.Language (SRAnalysis(..), SRConst(..))

-- | Pattern helpers
x, y, z :: Pattern SRTree
x = "x"
y = "y"
z = "z"

-- | Construct a non-variable pattern from an SRTree node
p :: SRTree (Pattern SRTree) -> Pattern SRTree
p = pat

pAdd, pSub, pMul, pDiv :: Pattern SRTree -> Pattern SRTree -> Pattern SRTree
pAdd a b = p (Bin Add a b)
pSub a b = p (Bin Sub a b)
pMul a b = p (Bin Mul a b)
pDiv a b = p (Bin Div a b)

pPow :: Pattern SRTree -> Pattern SRTree -> Pattern SRTree
pPow a b = p (Bin Power a b)

pConst :: Double -> Pattern SRTree
pConst v = p (Const v)

pLog, pExp, pRecip, pSqrt, pSq, pAbs :: Pattern SRTree -> Pattern SRTree
pLog   a = p (Uni Log a)
pExp   a = p (Uni Exp a)
pRecip a = p (Uni Recip a)
pSqrt  a = p (Uni Sqrt a)
pSq    a = p (Uni Square a)
pAbs   a = p (Uni Abs a)

-- | All rewrite rules for symbolic regression.
-- Commutativity of Add and Mul is handled by 'normalizeNode', not here.
srRewrites :: [Rewrite SRAnalysis SRTree]
srRewrites = identityRules ++ algebraicRules ++ functionRules

-- | Identity and annihilation rules
identityRules :: [Rewrite SRAnalysis SRTree]
identityRules =
    [ -- x + 0 = x
      pAdd x (pConst 0) := x
      -- 0 + x = x
    , pAdd (pConst 0) x := x
      -- x - 0 = x
    , pSub x (pConst 0) := x
      -- x * 1 = x
    , pMul x (pConst 1) := x
      -- 1 * x = x
    , pMul (pConst 1) x := x
      -- x * 0 = 0
    , pMul x (pConst 0) := pConst 0
      -- 0 * x = 0
    , pMul (pConst 0) x := pConst 0
      -- x / 1 = x
    , pDiv x (pConst 1) := x
      -- x ** 1 = x
    , pPow x (pConst 1) := x
      -- x ** 0 = 1
    , pPow x (pConst 0) := pConst 1
      -- x - x = 0
    , pSub x x := pConst 0
      -- x / x = 1  (when x != 0)
    , pDiv x x := pConst 1
    ]

-- | Core algebraic rules (associativity, distributivity, etc.)
-- Commutativity is NOT here — handled by normalizeNode.
algebraicRules :: [Rewrite SRAnalysis SRTree]
algebraicRules =
    [ -- Associativity of addition
      pAdd (pAdd x y) z := pAdd x (pAdd y z)
      -- Associativity of multiplication
    , pMul (pMul x y) z := pMul x (pMul y z)
      -- Distributivity: x*(y+z) = x*y + x*z
    , pMul x (pAdd y z) := pAdd (pMul x y) (pMul x z)
      -- Factoring: x*y + x*z = x*(y+z)
    , pAdd (pMul x y) (pMul x z) := pMul x (pAdd y z)
      -- Subtraction rearrangement
    , pSub x (pAdd y z) := pSub (pSub x y) z
    , pSub x (pSub y z) := pAdd (pSub x y) z
      -- Division rearrangement
    , pDiv (pMul x y) z := pMul (pDiv x z) y
      -- Power rules
    , pMul x x := pSq x
    , pSq (pSqrt x) := x
    ]

-- | Function simplification rules
functionRules :: [Rewrite SRAnalysis SRTree]
functionRules =
    [ -- log(exp(x)) = x
      pLog (pExp x) := x
      -- exp(log(x)) = x
    , pExp (pLog x) := x
      -- log(x*y) = log(x) + log(y)
    , pLog (pMul x y) := pAdd (pLog x) (pLog y)
      -- log(x**y) = y*log(x)
    , pLog (pPow x y) := pMul y (pLog x)
      -- recip(recip(x)) = x
    , pRecip (pRecip x) := x
      -- sqrt(x^2) = abs(x)
    , pSqrt (pSq x) := pAbs x
      -- abs(x*y) = abs(x)*abs(y)
    , pAbs (pMul x y) := pMul (pAbs x) (pAbs y)
    ]
