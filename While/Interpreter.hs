module Interpreter where 

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal 
evalVar v = do 
  varMap <- get 
  case Map.lookup v varMap of 
    Just rtVal -> pure rtVal
    Nothing    -> error $ "Undefined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal 
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var
evalExpr (Plus lhs rhs) = do 
  lhs' <- evalExpr lhs 
  rhs' <- evalExpr rhs
  case (lhs', rhs') of 
    (RTLit (LInt n), RTLit (LInt k)) -> pure $ RTLit (LInt $ n + k)
    _ -> error "Type error"

-- runState (evalExpr $ Plus (ELit $ LInt 2) (ELit $ LInt 3)) (Map.singleton (Var "v") (RTLit $ LInt 5))