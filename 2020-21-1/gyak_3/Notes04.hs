{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

import Control.Monad (ap, forM_, guard)
import Control.Monad.State (execState, State, put, get, modify)

-- Evaluation of expressions
data IntExpr = Value Int
             | Plus  IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Show, Eq, Ord)

expr1 :: IntExpr
expr1 = Value 10 `Plus` Value 5 -- (10 + 5)

expr2 :: IntExpr
expr2 = Value 10 `Times` expr1 -- (10 * (10 + 5))

expr3 :: IntExpr
expr3 = Value 10 `Div` Value 5 -- (10 `div` 5)

expr4 :: IntExpr
expr4 = Value 10 `Div` Value 0 -- (10 `div` 0)

expr5 :: IntExpr
expr5 = Plus (Div (Value 10) (Value 0)) (Value 1)

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

-- Define `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 150
--   evalIntExprMaybe expr3 == Just 2
--   evalIntExprMaybe expr4 == Nothing
-- Hint: first define
--   safeDiv :: Int -> Int -> Maybe Int
evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe (Value x)  = return x -- same as Just x
-- evalIntExprMaybe (Plus x y) = (liftM2 (+)) (evalIntExprMaybe x) (evalIntExprMaybe y)
-- evalIntExprMaybe (Plus x y) = case (evalIntExprMaybe x, evalIntExprMaybe y) of
--   (Just a, Just b) -> return (a+b)
--   _                -> Nothing
evalIntExprMaybe (Plus x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  return (x' + y')
evalIntExprMaybe (Times x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  return (x' * y')
-- evalIntExprMaybe (Plus x y) = case (evalIntExprMaybe x, evalIntExprMaybe y) of
--   (Just a, Just b) | b /= 0 -> return (a+b)
--   _                -> Nothing
-- evalIntExprMaybe (Div x y) = do
--   x' <- evalIntExprMaybe x
--   y' <- evalIntExprMaybe y -- y' :: Int
--   if y' == 0 then Nothing
--   else return (x' `div` y')
evalIntExprMaybe (Div x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y -- y' :: Int
  if y' == 0 then Nothing else Just () -- this can also be written guard (y' /= 0)
  return (x' `div` y')



-- Some operations on monads
liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f ma = do
--   a <- ma
--   return (f a)
liftM = fmap

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

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f []     _      = []
zipWith' f _      []     = []
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f []     _      = return []
zipWithM f _      []     = return []
zipWithM f (a:as) (b:bs) = liftM2 (:) (f a b) (zipWithM f as bs)
-- zipWithM f (a:as) (b:bs) = do
--   c <- (f a b) 
--   cs <- zipWithM f as bs
--   return (c:cs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f e []     = e
foldl' f e (x:xs) = foldl' f (f e x) xs

foldr' :: (b -> a -> b) -> b -> [a] -> b
foldr' f e []     = e
foldr' f e (x:xs) = f (foldr' f e xs) x

foldrM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldrM' f e [] = return e
foldrM' f e (x:xs) = do
  y <- foldrM' f e xs
  f y x

-- Define using the State monad:

-- Example:
--   x = 1
--   for i from 1 to n
--     x = x + 1
ex :: Integer -> State Integer ()
ex n = do
  put 1 -- x = 1
  forM_ [1..n] $ \i -> do -- for i from 1 to n
    x <- get 
    put (x+1) -- x = x+1

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