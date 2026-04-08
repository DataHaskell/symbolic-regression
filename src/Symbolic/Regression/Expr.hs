{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

{- | Expression functor for hegg's e-graph.

Defines the core symbolic regression language: a fixed-point expression tree
with binary operations, unary operations, variables, parameters, and literals.
Uses hegg's 'Fix' directly as the recursive wrapper.
-}
module Symbolic.Regression.Expr (
    -- * Expression functor
    ExprF (..),
    BinOp (..),
    UnOp (..),

    -- * Re-exports
    PolyMap,
    Fix (..),
    cata,

    -- * Smart constructors
    var,
    param,
    lit,

    -- * Type aliases
    PVector,
    Features,
) where

import Data.Equality.Graph.Poly (PolyMap)
import Data.Equality.Utils (Fix (..), cata)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{- | Expression functor. Each constructor is one node in the expression DAG.
The type parameter @a@ is the recursive position (child reference).

'SumF' and 'ProdF' represent AC-canonical addition and multiplication as
sorted lists of children (multisets). Binary 'Add'/'Mul' via 'BinF' are
converted to these on e-graph insertion by 'exprNormalize'.
-}
data ExprF a
    = -- | Feature variable by column index
      VarF {-# UNPACK #-} !Int
    | -- | Learnable parameter by index
      ParamF {-# UNPACK #-} !Int
    | -- | Numeric literal
      LitF {-# UNPACK #-} !Double
    | -- | Binary operation (Sub, Div, Pow only in e-graph)
      BinF !BinOp !a !a
    | -- | Unary operation
      UnF !UnOp !a
    | -- | AC addition: sorted list of summands
      SumF [a]
    | -- | AC multiplication: sorted list of factors
      ProdF [a]
    | {- | Polynomial container: multiset of multisets.
      Encodes a polynomial as a single e-node; distributivity is structural.
      The ClassId keys inside PolyMap are NOT children in the Traversable sense;
      congruence is maintained via exprNormalize / addWithNorm.
      -}
      PolyF !PolyMap
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary operations.
data BinOp = Add | Sub | Mul | Div | Pow
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Unary operations.
data UnOp = Neg | Abs | Recip | Sq | Cube | Sqrt | Exp | Log | Sin | Cos
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Feature variable.
var :: Int -> Fix ExprF
var = Fix . VarF

-- | Learnable parameter.
param :: Int -> Fix ExprF
param = Fix . ParamF

-- | Numeric literal.
lit :: Double -> Fix ExprF
lit = Fix . LitF

-- | Parameter / target vector.
type PVector = VU.Vector Double

-- | Feature matrix: one row per observation, each row an unboxed vector.
type Features = V.Vector (VU.Vector Double)

------------------------------------------------------------------------
-- Num / Fractional / Floating instances (with constant folding)
------------------------------------------------------------------------

instance Num (Fix ExprF) where
    Fix (LitF 0) + r = r
    l + Fix (LitF 0) = l
    Fix (LitF a) + Fix (LitF b) = lit (a + b)
    l + r = Fix (BinF Add l r)
    {-# INLINE (+) #-}

    l - Fix (LitF 0) = l
    Fix (LitF 0) - r = negate r
    Fix (LitF a) - Fix (LitF b) = lit (a - b)
    l - r = Fix (BinF Sub l r)
    {-# INLINE (-) #-}

    Fix (LitF 0) * _ = lit 0
    _ * Fix (LitF 0) = lit 0
    Fix (LitF 1) * r = r
    l * Fix (LitF 1) = l
    Fix (LitF a) * Fix (LitF b) = lit (a * b)
    l * r = Fix (BinF Mul l r)
    {-# INLINE (*) #-}

    abs = Fix . UnF Abs
    negate (Fix (LitF x)) = lit (negate x)
    negate t = lit (-1) * t
    signum (Fix (LitF x)) = lit (signum x)
    signum _ = lit 0
    fromInteger = lit . fromInteger

instance Fractional (Fix ExprF) where
    _ / Fix (LitF 0) = lit (1 / 0)
    l / Fix (LitF 1) = l
    Fix (LitF a) / Fix (LitF b) = lit (a / b)
    l / r = Fix (BinF Div l r)
    {-# INLINE (/) #-}

    recip = Fix . UnF Recip
    fromRational = lit . fromRational

instance Floating (Fix ExprF) where
    pi = lit pi
    exp = Fix . UnF Exp
    log = Fix . UnF Log
    sqrt = Fix . UnF Sqrt
    sin = Fix . UnF Sin
    cos = Fix . UnF Cos
    tan x = sin x / cos x
    asin = error "asin not supported"
    acos = error "acos not supported"
    atan = error "atan not supported"
    sinh x = (exp x - exp (negate x)) / 2
    cosh x = (exp x + exp (negate x)) / 2
    tanh x = sinh x / cosh x
    asinh = error "asinh not supported"
    acosh = error "acosh not supported"
    atanh = error "atanh not supported"

    _ ** Fix (LitF 0) = lit 1
    l ** Fix (LitF 1) = l
    l ** r = Fix (BinF Pow l r)
    {-# INLINE (**) #-}

    logBase l r = log r / log l
