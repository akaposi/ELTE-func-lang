{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

import Control.Monad (ap)
import Control.Monad.State

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
evalIntExpr (Value x)   = x
evalIntExpr (Plus x y)  = (evalIntExpr x) + (evalIntExpr y)
evalIntExpr (Times x y) = (evalIntExpr x) * (evalIntExpr y)
evalIntExpr (Div x y)   = (evalIntExpr x) `div` (evalIntExpr y)

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y | y == 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 150
--   evalIntExprMaybe expr3 == Just 2
--   evalIntExprMaybe expr4 == Nothing
evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe (Value x)  = return x
evalIntExprMaybe (Plus x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  return (x' + y')
-- evalIntExprMaybe (Plus x y) = liftM2 (+) (evalIntExprMaybe x) (evalIntExprMaybe y)
evalIntExprMaybe (Times x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  return (x' * y')
evalIntExprMaybe (Div x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  safeDiv x' y'
-- Alternative definition:
-- evalIntExprMaybe (Div x y) = do
--   x' <- evalIntExprMaybe x
--   y' <- evalIntExprMaybe y
--   -- guard (y' /= 0)
--   if y' == 0 then Nothing else Just ()
--   return (x' `div` y')

-- Some operations on monads
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return (f a)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return (f a b c)

foldM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM f e [] = return e
foldM' f e (x:xs) = do
  e' <- f e x 
  foldM' f e' xs

-- Define using the State monad:

-- impFactorial should be a translation of the imperative program
--    x = 1
--    for i from 1 to n
--      x = x * i

impFactorial :: Integer -> State Integer ()
impFactorial n = do
  put 1                   -- x = 1
  forM_ [1..n] $ \i -> do -- for i from 1 to n
    modify (\x -> x * i)  --   x = x * i

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    a = 1
--    b = 1
--    for i from 1 to n
--      (a, b) = (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = do
  put (1, 1)
  forM_ [1..n] $ \i -> do
    modify (\(a,b) -> (b, a+b))

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))