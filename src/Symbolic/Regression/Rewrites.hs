{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Algebraic rewrite rules for symbolic regression using hegg's API.

Commutativity rules are /omitted/ — they are handled structurally by
'normalizeNode' in "Symbolic.Regression.Language".
-}
module Symbolic.Regression.Rewrites (
    srRewrites,
    srRewritesNoDist,
) where

import Data.Equality.Matching
import Data.Equality.Saturation.Rewrites

import Symbolic.Regression.Expr (BinOp (..), ExprF (..), UnOp (..))
import Symbolic.Regression.Language (SRAnalysis (..))

-- | Pattern variables
x, y, z :: Pattern ExprF
x = "x"
y = "y"
z = "z"

-- | Construct a non-variable pattern from an ExprF node
p :: ExprF (Pattern ExprF) -> Pattern ExprF
p = pat

pAdd, pSub, pMul, pDiv :: Pattern ExprF -> Pattern ExprF -> Pattern ExprF
pAdd a b = p (BinF Add a b)
pSub a b = p (BinF Sub a b)
pMul a b = p (BinF Mul a b)
pDiv a b = p (BinF Div a b)

pPow :: Pattern ExprF -> Pattern ExprF -> Pattern ExprF
pPow a b = p (BinF Pow a b)

pLit :: Double -> Pattern ExprF
pLit v = p (LitF v)

pNeg, pLog, pExp, pRecip, pSqrt, pSq, pAbs :: Pattern ExprF -> Pattern ExprF
pNeg a = p (UnF Neg a)
pLog a = p (UnF Log a)
pExp a = p (UnF Exp a)
pRecip a = p (UnF Recip a)
pSqrt a = p (UnF Sqrt a)
pSq a = p (UnF Sq a)
pAbs a = p (UnF Abs a)

{- | All rewrite rules for symbolic regression.
Commutativity of Add and Mul is handled by 'normalizeNode', not here.
-}
srRewrites :: [Rewrite SRAnalysis ExprF]
srRewrites = identityRules ++ algebraicRules ++ functionRules

{- | Restricted rewrite set excluding distributivity.
Avoids O(n²) blowup when saturating a SumF with many children.
Used during boosting training; full 'srRewrites' is reserved for final distillation.
-}
srRewritesNoDist :: [Rewrite SRAnalysis ExprF]
srRewritesNoDist = identityRules ++ algebraicRulesNoDist ++ functionRules

-- | Algebraic rules without distributivity.
algebraicRulesNoDist :: [Rewrite SRAnalysis ExprF]
algebraicRulesNoDist =
    [ pMul x x := pSq x
    , pSq (pNeg x) := pSq x
    , pSq (pSqrt x) := x
    ]

identityRules :: [Rewrite SRAnalysis ExprF]
identityRules =
    [ pAdd x (pLit 0) := x
    , pAdd (pLit 0) x := x
    , pSub x (pLit 0) := x
    , pMul x (pLit 1) := x
    , pMul (pLit 1) x := x
    , pMul x (pLit 0) := pLit 0
    , pMul (pLit 0) x := pLit 0
    , pDiv x (pLit 1) := x
    , pPow x (pLit 1) := x
    , pPow x (pLit 0) := pLit 1
    , pSub x x := pLit 0
    , pDiv x x := pLit 1
    ]

-- Associativity, Sub rearrangement, and Div rearrangement are now
-- handled structurally by normalizeInContext (AC flattening, Sub/Div elimination).
algebraicRules :: [Rewrite SRAnalysis ExprF]
algebraicRules =
    [ -- Distributivity removed: now handled structurally by PolyF container.
      -- Power rules
      pMul x x := pSq x
    , pSq (pNeg x) := pSq x
    , pSq (pSqrt x) := x
    ]

pSin, pCos :: Pattern ExprF -> Pattern ExprF
pSin a = p (UnF Sin a)
pCos a = p (UnF Cos a)

functionRules :: [Rewrite SRAnalysis ExprF]
functionRules =
    [ pLog (pExp x) := x
    , pExp (pLog x) := x
    , pLog (pMul x y) := pAdd (pLog x) (pLog y)
    , pLog (pPow x y) := pMul y (pLog x)
    , -- Recip simplifications
      pRecip (pRecip x) := x
    , pDiv x (pRecip y) := pMul x y -- x / (1/y) → x*y
    , pRecip (pMul x (pRecip y)) := pDiv y x -- 1/(x * 1/y) → y/x
    , pMul x (pRecip x) := pLit 1 -- x * (1/x) → 1
    , -- Even/odd symmetry
      pCos (pNeg x) := pCos x -- cos(-x) → cos(x)
    , pSin (pNeg x) := pNeg (pSin x) -- sin(-x) → -sin(x)
    , pAbs (pNeg x) := pAbs x -- |−x| → |x|
    , -- Neg simplifications
      pNeg (pNeg x) := x -- −(−x) → x
    , pMul (pNeg x) (pNeg y) := pMul x y -- (−x)(−y) → xy
    , -- Root/power
      pSqrt (pSq x) := pAbs x
    , pAbs (pMul x y) := pMul (pAbs x) (pAbs y)
    ]
