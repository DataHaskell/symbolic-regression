{-# LANGUAGE BangPatterns #-}


{-# LANGUAGE UndecidableInstances #-}

{- | Polynomial normal form for symbolic expressions.

A polynomial is a sum of monomials, where each monomial is a coefficient
times a multiset of atoms.  Atoms are opaque subexpressions (variables,
transcendentals) that we don't expand through.

The representation guarantees that identical monomials are always merged:
@2*x + 3*x@ is represented as a single entry @{x→1} → 5.0@.

Usage:

@
polySimplify :: Fix ExprF -> Fix ExprF
polySimplify = fromPoly . toPoly
@
-}
module Symbolic.Regression.Poly (
    Poly (..),
    toPoly,
    fromPoly,
    polySimplify,
    magnitudePrune,
) where

import Data.Equality.Utils (Fix (..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Symbolic.Regression.Expr
import Symbolic.Regression.Expr.Eval (evalTree)

------------------------------------------------------------------------
-- Atom: ordered wrapper around Fix ExprF for use as Map keys
------------------------------------------------------------------------

{- | An opaque subexpression used as a polynomial atom.
  Wraps Fix ExprF with Eq/Ord based on structural comparison via Show.
-}
newtype Atom = Atom {unAtom :: Fix ExprF}

instance Eq Atom where
    Atom a == Atom b = show a == show b

instance Ord Atom where
    compare (Atom a) (Atom b) = compare (show a) (show b)

instance Show Atom where
    show (Atom a) = show a

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

{- | A polynomial: map from monomial key (multiset of atoms) to coefficient.
  Invariant: no entry with coefficient ≈ 0.
-}
newtype Poly = Poly (Map.Map (Map.Map Atom Int) Double)
    deriving (Show)

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Constant polynomial.
constPoly :: Double -> Poly
constPoly 0 = Poly Map.empty
constPoly c = Poly (Map.singleton Map.empty c)

-- | Single atom with coefficient 1.
atomPoly :: Fix ExprF -> Poly
atomPoly a = Poly (Map.singleton (Map.singleton (Atom a) 1) 1.0)

------------------------------------------------------------------------
-- Arithmetic
------------------------------------------------------------------------

-- | Add two polynomials (merge monomials, sum coefficients).
addPoly :: Poly -> Poly -> Poly
addPoly (Poly a) (Poly b) = Poly (Map.unionWith (+) a b)

-- | Negate all coefficients.
negatePoly :: Poly -> Poly
negatePoly (Poly m) = Poly (Map.map negate m)

-- | Subtract: a - b.
subPoly :: Poly -> Poly -> Poly
subPoly a b = addPoly a (negatePoly b)

-- | Multiply two polynomials (distribute and collect).
mulPoly :: Poly -> Poly -> Poly
mulPoly (Poly as) (Poly bs) =
    Poly $
        Map.fromListWith
            (+)
            [ (Map.unionWith (+) ka kb, ca * cb)
            | (ka, ca) <- Map.toList as
            , (kb, cb) <- Map.toList bs
            ]

-- | Scale all coefficients by a constant.
scalePoly :: Double -> Poly -> Poly
scalePoly s (Poly m) = Poly (Map.map (* s) m)

-- | Number of monomials in the polynomial.
polySize :: Poly -> Int
polySize (Poly m) = Map.size m

------------------------------------------------------------------------
-- Conversion: ExprF → Poly
------------------------------------------------------------------------

{- | Is this expression "polynomial" — safe to expand through?
  Variables, constants, Neg, Sq, sums, and products of polynomial parts.
-}
isPoly :: Fix ExprF -> Bool
isPoly (Fix (VarF _)) = True
isPoly (Fix (LitF _)) = True
isPoly (Fix (ParamF _)) = True
isPoly (Fix (UnF Neg e)) = isPoly e
isPoly (Fix (UnF Sq e)) = isPoly e
isPoly (Fix (UnF Cube e)) = isPoly e
isPoly (Fix (SumF xs)) = all isPoly xs
isPoly (Fix (ProdF xs)) = all isPoly xs
isPoly (Fix (BinF Add l r)) = isPoly l && isPoly r
isPoly (Fix (BinF Sub l r)) = isPoly l && isPoly r
isPoly (Fix (BinF Mul l r)) = isPoly l && isPoly r
isPoly _ = False

{- | Convert an expression to polynomial normal form.
  Polynomial subexpressions are expanded; transcendentals become opaque atoms
  (with their arguments recursively simplified).
-}
toPoly :: Fix ExprF -> Poly
toPoly (Fix (LitF c)) = constPoly c
toPoly (Fix (VarF i)) = atomPoly (var i)
toPoly (Fix (ParamF i)) = atomPoly (param i)
-- Additive cases
toPoly (Fix (SumF xs)) = foldl addPoly (constPoly 0) (map toPoly xs)
toPoly (Fix (BinF Add l r)) = addPoly (toPoly l) (toPoly r)
toPoly (Fix (BinF Sub l r)) = subPoly (toPoly l) (toPoly r)
-- Negation
toPoly (Fix (UnF Neg e)) = negatePoly (toPoly e)
-- Multiplicative cases (with blowup guard)
toPoly (Fix (ProdF xs)) = foldl mulSafe (constPoly 1) (map toPoly xs)
toPoly (Fix (BinF Mul l r)) = mulSafe (toPoly l) (toPoly r)
-- Sq and Cube: expand if polynomial
toPoly (Fix (UnF Sq e))
    | isPoly e = let p = toPoly e in mulSafe p p
    | otherwise = atomPoly (Fix (UnF Sq (polySimplify e)))
toPoly (Fix (UnF Cube e))
    | isPoly e = let p = toPoly e in mulSafe p (mulSafe p p)
    | otherwise = atomPoly (Fix (UnF Cube (polySimplify e)))
-- Div and Pow: don't expand, treat as opaque
toPoly (Fix (BinF Div l r)) =
    atomPoly (Fix (BinF Div (polySimplify l) (polySimplify r)))
toPoly (Fix (BinF Pow l r)) =
    atomPoly (Fix (BinF Pow (polySimplify l) (polySimplify r)))
-- All other unary ops: opaque atom with simplified argument
toPoly (Fix (UnF op e)) = atomPoly (Fix (UnF op (polySimplify e)))
-- Fallback for any other node
toPoly e = atomPoly e

-- | Multiply with blowup guard.
mulSafe :: Poly -> Poly -> Poly
mulSafe a b
    | polySize a * polySize b > 200 =
        let !ea = fromPoly a; !eb = fromPoly b
         in atomPoly (Fix (BinF Mul ea eb))
    | otherwise = mulPoly a b

------------------------------------------------------------------------
-- Conversion: Poly → ExprF
------------------------------------------------------------------------

{- | Convert polynomial back to an expression tree.
  Drops near-zero monomials, rebuilds as SumF of ProdF terms.
-}
fromPoly :: Poly -> Fix ExprF
fromPoly (Poly m) =
    let terms =
            [ buildMono c atoms
            | (atoms, c) <- Map.toList m
            , abs c > 1e-9
            ]
     in case terms of
            [] -> lit 0
            [x] -> x
            _ -> Fix (SumF terms)

-- | Build a single monomial expression from coefficient and atom multiset.
buildMono :: Double -> Map.Map Atom Int -> Fix ExprF
buildMono c atoms
    | Map.null atoms = lit c
    | otherwise =
        let expandedAtoms = concatMap (\(Atom a, n) -> replicate n a) (Map.toList atoms)
            product_ = case expandedAtoms of
                [single] -> single
                xs -> Fix (ProdF xs)
         in if abs (c - 1.0) < 1e-12
                then product_
                else Fix (ProdF (lit c : expandedAtoms))

------------------------------------------------------------------------
-- Top-level simplifier
------------------------------------------------------------------------

{- | Simplify an expression via polynomial normal form.
  Expands polynomial products, collects like monomials, drops near-zeros.
-}
polySimplify :: Fix ExprF -> Fix ExprF
polySimplify = fromPoly . toPoly

------------------------------------------------------------------------
-- Loss-based pruning
------------------------------------------------------------------------

{- | Drop monomials whose removal does not significantly increase MSE.
For each monomial, temporarily remove it, rebuild the expression,
evaluate on training data, and check if MSE increases by more than
@tolerance * baseMSE@.  If not, the term is dropped.

This is more principled than magnitude pruning: a term that is
individually small can still matter if it counterbalances another term.
-}
magnitudePrune :: Double -> Features -> Fix ExprF -> Fix ExprF
magnitudePrune tolerance xTrain expr =
    let Poly m = toPoly expr
        entries = Map.toList m
        basePreds = evalTree xTrain VU.empty expr
        -- Use mean of squared predictions as baseline (not MSE against target,
        -- since we don't have the target here — we measure prediction change)
        baseNorm = VU.sum (VU.map (\v -> v * v) basePreds) / fromIntegral (VU.length basePreds)
     in if baseNorm < 1e-15 || length entries <= 1
            then expr
            else
                let kept = filter (termMatters baseNorm basePreds entries) entries
                 in if null kept
                        then expr
                        else fromPoly (Poly (Map.fromList kept))
  where
    termMatters baseNorm bPreds allEntries (key, _coeff) =
        let
            -- Build expression without this term
            without = Poly (Map.fromList [(k, c) | (k, c) <- allEntries, k /= key])
            withoutExpr = fromPoly without
            withoutPreds = evalTree xTrain VU.empty withoutExpr
            -- Measure change in predictions
            delta =
                VU.sum (VU.zipWith (\a b -> (a - b) * (a - b)) bPreds withoutPreds)
                    / fromIntegral (VU.length bPreds)
         in
            -- Keep the term if removing it changes predictions by > tolerance relative to baseline
            delta / baseNorm > tolerance
