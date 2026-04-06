{-# LANGUAGE LambdaCase #-}
-- | Pretty-printing expressions in various formats.
module Symbolic.Regression.Expr.Print
    ( showExpr
    , showExprWithVars
    , showPython
    , showLatex
    , showLatexWithVars
    , showBinOp
    ) where

import Data.Char (toLower)
import Symbolic.Regression.Expr

-- | Math notation.
showExpr :: Fix ExprF -> String
showExpr = cata alg
  where
    alg = \case
        VarF i      -> 'x' : show i
        ParamF i    -> 't' : show i
        LitF c      -> show c
        BinF op l r -> concat ["(", l, " ", showBinOp op, " ", r, ")"]
        UnF f t     -> concat [showUnOp f, "(", t, ")"]

-- | Math notation with named variables.
showExprWithVars :: [String] -> Fix ExprF -> String
showExprWithVars varnames = cata alg
  where
    alg = \case
        VarF i      -> varnames !! i
        ParamF i    -> 't' : show i
        LitF c      -> show c
        BinF op l r -> concat ["(", l, " ", showBinOp op, " ", r, ")"]
        UnF f t     -> concat [showUnOp f, "(", t, ")"]

-- | NumPy notation.
showPython :: Fix ExprF -> String
showPython = cata alg
  where
    alg = \case
        VarF i      -> concat ["x[:, ", show i, "]"]
        ParamF i    -> concat ["t[", show i, "]"]
        LitF c      -> show c
        BinF Pow l r -> concat [l, " ** ", r]
        BinF op l r -> concat ["(", l, " ", showBinOp op, " ", r, ")"]
        UnF f t     -> concat [pyFun f, "(", t, ")"]

    pyFun Neg   = "-"
    pyFun Abs   = "np.abs"
    pyFun Recip = "np.reciprocal"
    pyFun Sq    = "np.square"
    pyFun Cube  = "np.power"  -- np.power(x, 3) handled differently
    pyFun Sqrt  = "np.sqrt"
    pyFun Exp   = "np.exp"
    pyFun Log   = "np.log"
    pyFun Sin   = "np.sin"
    pyFun Cos   = "np.cos"

-- | LaTeX notation.
showLatex :: Fix ExprF -> String
showLatex = cata alg
  where
    alg = \case
        VarF i      -> concat ["x_{", show i, "}"]
        ParamF i    -> concat ["\\theta_{", show i, "}"]
        LitF c      -> show c
        BinF Pow l r -> concat ["{", l, "^{", r, "}}"]
        BinF Mul l r -> concat ["\\left(", l, " \\cdot ", r, "\\right)"]
        BinF Div l r -> concat ["\\frac{", l, "}{", r, "}"]
        BinF op l r -> concat ["\\left(", l, " ", showBinOp op, " ", r, "\\right)"]
        UnF Abs t   -> concat ["\\left|", t, "\\right|"]
        UnF Recip t -> concat ["\\frac{1}{", t, "}"]
        UnF f t     -> concat [latexFun f, "\\left(", t, "\\right)"]

-- | LaTeX notation with named variables.
showLatexWithVars :: [String] -> Fix ExprF -> String
showLatexWithVars varnames = cata alg
  where
    alg = \case
        VarF i      -> concat ["\\operatorname{", varnames !! i, "}"]
        ParamF i    -> concat ["\\theta_{", show i, "}"]
        LitF c      -> show c
        BinF Pow l r -> concat ["{", l, "^{", r, "}}"]
        BinF Mul l r -> concat ["\\left(", l, " \\cdot ", r, "\\right)"]
        BinF Div l r -> concat ["\\frac{", l, "}{", r, "}"]
        BinF op l r -> concat ["\\left(", l, " ", showBinOp op, " ", r, "\\right)"]
        UnF Abs t   -> concat ["\\left|", t, "\\right|"]
        UnF Recip t -> concat ["\\frac{1}{", t, "}"]
        UnF f t     -> concat [latexFun f, "\\left(", t, "\\right)"]

showBinOp :: BinOp -> String
showBinOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Pow -> "^"
{-# INLINE showBinOp #-}

showUnOp :: UnOp -> String
showUnOp = \case
    Neg   -> "-"
    Abs   -> "Abs"
    Recip -> "Recip"
    Sq    -> "Square"
    Cube  -> "Cube"
    Sqrt  -> "Sqrt"
    Exp   -> "Exp"
    Log   -> "Log"
    Sin   -> "Sin"
    Cos   -> "Cos"

latexFun :: UnOp -> String
latexFun f = "\\operatorname{" ++ map toLower (showUnOp f) ++ "}"
