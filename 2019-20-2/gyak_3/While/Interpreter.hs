module Interpreter where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax

-- RuntimeValue
data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit lit = RTLit <$> pure lit
{-
evalLit lit = pure $ RTLit lit
-}

evalVar :: Var -> Eval RTVal
evalVar v = do
  varMapping <- get
  case Map.lookup v varMapping of
    Just rtVal -> pure rtVal
    Nothing    -> error $ "Undefined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var
evalExpr (Plus lhs rhs) = do
  lhsRTVal <- evalExpr lhs
  rhsRTVal <- evalExpr rhs
  case (lhsRTVal, rhsRTVal) of
    (RTLit (LInt lhsVal), RTLit (LInt rhsVal))
      -> pure $ RTLit $ LInt $ lhsVal + rhsVal
    _ -> error "Type error"

-- runState (evalExpr $ Plus (ELit $ LInt 2) (ELit $ LInt 3)) (Map.singleton (Var "v") (RTLit $ LInt 5))
