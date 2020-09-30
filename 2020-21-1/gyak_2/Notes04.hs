{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

import Control.Monad (ap)

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Show, Eq, Ord)

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5

expr2 :: IntExpr
expr2 = Value 10 `Times` expr1

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 5

expr4 :: IntExpr
expr4 = Value 10 `Div` Value 0

-- Define `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 150
--   evalIntExpr expr3 == 2
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr = undefined

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 150
--   evalIntExprMaybe expr3 == Just 2
--   evalIntExprMaybe expr4 == Nothing
evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe = undefined


-- Some operations on monads
liftM :: Monad m => (a -> b) -> m a -> m b
liftM = undefined

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 = undefined

liftM3 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM3 = undefined

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM = undefined