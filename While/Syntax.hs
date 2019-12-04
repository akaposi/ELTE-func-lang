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
  = Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  deriving (Eq, Ord, Show)
