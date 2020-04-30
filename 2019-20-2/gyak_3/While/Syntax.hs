module Syntax where

data Lit
  = LBool Bool
  | LInt Int
  deriving (Eq, Ord, Show)

type Name = String

newtype Var = Var Name
  deriving (Eq, Ord, Show)

data Expr
  -- atoms
  = ELit Lit
  | EVar Var
  -- arithmetic
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  -- logical
  | And Expr Expr
  | Eq Expr Expr
  | LEq Expr Expr
  | Not Expr
  deriving (Eq, Ord, Show)

data Statement
  = Skip
  | Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  deriving (Eq, Ord, Show)

{-
Program as string:

if (x <= 1)
  x := 2
else
  y := 3;
  x := y

Abstract Syntax Tree (AST):

If (LEq (EVar (Var "x")) (ELit (LInt 1)))
  (Assign (Var "x") (ELit (LInt 2)))
  (Seq
    (Assign (Var "y") (ELit (LInt 3)))
    (Assign (Var "x") (Evar (Var "x"))))

-}
