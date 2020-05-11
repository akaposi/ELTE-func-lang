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

evalStatement :: Expr -> Eval ()
evalStatement Skip = pure ()
evalStatement (Assign var expr) = do
  res <- evalExpr
  varMapping <- get
  let varMapping' = Map.insert var res varMapping
  put varMapping'
evalStatement (If cond lhs rhs) = do
  condRTVal <- evalExpr cond
  case condRTVal of
    RTLit (LBool True)  -> evalStatement lhs
    RTLit (LBool False) -> evalStatement rhs
    _ -> error "Type error"
evalStatement (Seq lhs rhs) = do
  evalStatement lhs
  evalStatement rhs
evalStatement loop@(While cond body) = do
  condRTVal <- evalExpr cond
  case condRTVal of
    RTLit (LBool True) -> do
      evalStatement body
      evalStatement loop
    RTLit (LBool False) -> do
      pure ()
    _ -> error "Type error"

-- runState (evalExpr $ Plus (ELit $ LInt 2) (ELit $ LInt 3)) (Map.singleton (Var "v") (RTLit $ LInt 5))
