module Interpreter where

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit lit = undefined

evalVar :: Var -> Eval RTVal
evalVar v = undefined

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var
evalExpr (Plus lhs rhs) = undefined

-- runState (evalExpr $ Plus (ELit $ LInt 2) (ELit $ LInt 3)) (Map.singleton (Var "v") (RTLit $ LInt 5))
