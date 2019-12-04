module Interpreter where

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax 

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit (LInt n) = pure (RTLit (LInt n))

evalVar :: Var -> Eval RTVal 
evalVar v = do 
  varMap <- get
  case Map.lookup v varMap of 
    Just rtVal -> pure rtVal
    Nothing    -> error $ "Variable not found in environment: " ++ show v

evalExpr :: Expr -> Eval RTVal 
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var