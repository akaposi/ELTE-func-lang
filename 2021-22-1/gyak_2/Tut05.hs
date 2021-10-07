{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Tut05 where
import Prelude hiding (mapM)
import Control.Monad (join)

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Show, Eq, Ord)

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5 -- (10 + 5)

expr2 :: IntExpr
expr2 = Value 10 `Div` Value 5 -- (10 `div` 5)

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 0 -- (10 `div` 0)

expr4 :: IntExpr
expr4 = Div (Value 1) (Plus (Value 10) (Value (-10))) -- 1 / (10 - 10)

-- Define an evaluation function `evalIntExpr :: IntExpr -> Int`
-- Examples: 
--   evalIntExpr expr1 == 15
--   evalIntExpr expr2 == 2
--   evalIntExpr expr3 == ???
--   evalIntExpr expr4 == ???

evalIntExpr :: IntExpr -> Int
evalIntExpr = undefined 

-- Define a "safe" evaluation function `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 2
--   evalIntExprMaybe expr3 == Nothing
--   evalIntExprMaybe expr4 == Nothing

-- Hint: define a function `safeDiv :: Int -> Int -> Maybe Int`.
--  that returns Nothing instead of dividing by zero.

evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe = undefined

-- General monadic operations (in Control.Monad)

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = undefined

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f x y = undefined

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f x y z = undefined

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = undefined
mapM f (x:xs) = undefined 

-- Other monadic functions:

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

-- forever ma  in Haskell   ~    while(true) { ma } 
forever :: Monad m => m a -> m b
forever ma = do
  ma
  forever ma

-- whileM cond ma        ~    while(cond) { ma }
whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond ma = undefined

-- forM xs f       ~    for(x in xs) { f x }
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM = undefined
