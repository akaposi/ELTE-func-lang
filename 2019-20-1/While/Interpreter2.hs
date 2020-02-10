module Interpreter2 where 

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Data.Map (Map(..))
import qualified Data.Map as Map

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = StateT (Map Var RTVal) (ExceptT String (Identity)) a

evalEval :: Eval a -> Map Var RTVal -> Either String a 
evalEval eval varMap = runExcept $ evalStateT eval varMap
-- evalEval eval varMap = runIdentity $ runExceptT $ evalStateT eval varMap
-- runStateT eval varMap :: ExceptT (RTVal, Map Var RTVal)

execEval :: Eval a -> Map Var RTVal -> Either String (Map Var RTVal) 
execEval eval varMap = runExcept $ execStateT eval varMap

runEval :: Eval a -> Map Var RTVal -> Either String (a, Map Var RTVal) 
runEval eval varMap = runExcept $ runStateT eval varMap

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal 
evalVar v = do 
  varMap <- get 
  case Map.lookup v varMap of 
    Just rtVal -> pure rtVal
    Nothing    -> throwError $ "Undefined variable: " ++ show v

evalExpr :: Expr -> Eval RTVal 
evalExpr (ELit lit) = evalLit lit
evalExpr (EVar var) = evalVar var
evalExpr (Plus lhs rhs) = do 
  lhs' <- evalExpr lhs 
  rhs' <- evalExpr rhs
  case (lhs', rhs') of 
    (RTLit (LInt n), RTLit (LInt k)) -> pure $ RTLit (LInt $ n + k)
    _ -> throwError $ "Type error"

foo :: Either String RTVal 
foo = flip evalEval mempty $ do
  catchError (evalVar (Var "x")) $ \e -> 
    pure (RTLit $ LInt 5)