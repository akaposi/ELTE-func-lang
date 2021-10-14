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
evalIntExpr (Value x)  = x
evalIntExpr (Plus x y) = evalIntExpr x + evalIntExpr y
evalIntExpr (Div x y)  = evalIntExpr x `div` evalIntExpr y

-- Define a "safe" evaluation function `evalIntExprMaybe :: IntExpr -> Maybe Int`
-- Examples: 
--   evalIntExprMaybe expr1 == Just 15
--   evalIntExprMaybe expr2 == Just 2
--   evalIntExprMaybe expr3 == Nothing
--   evalIntExprMaybe expr4 == Nothing

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

evalIntExprMaybe :: IntExpr -> Maybe Int
evalIntExprMaybe (Value x)  = Just x

-- evalIntExprMaybe (Plus x y) = liftM2 (+) mx my
evalIntExprMaybe (Plus x y) = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  Just (x'+y')

-- evalIntExprMaybe (Plus x y) = (+) <$> (evalIntExprMaybe x) <*> (evalIntExprMaybe y)

evalIntExprMaybe (Div x y)  = do
  x' <- evalIntExprMaybe x
  y' <- evalIntExprMaybe y
  safeDiv x' y'

-- evalIntExprMaybe (Div x y)  = join (safeDiv <$> (evalIntExprMaybe x) <*> (evalIntExprMaybe y))

-- General monadic operations (in Control.Monad)

liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f x = fmap f x
liftM f x = do
  x' <- x
  return (f x')

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f x y = do
  x' <- x
  y' <- y
  return (f x' y')

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f x y z = do
  x' <- x
  y' <- y
  z' <- z
  return (f x' y' z')

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = do
  y <- f x  -- f x :: m b
            -- y   :: b
  ys <- mapM f xs
  return (y:ys)
  
-- Other monadic functions:

f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = do
  a <- ma
  b <- mb
  pure (a,b)
-- f2 = liftM2 (,)

f3 :: Monad m => m (m a) -> m a
f3 mma = do
  ma <- mma
  ma

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 mf ma = do
  f <- mf
  a <- ma
  pure (f a)
-- f4 = liftM2 ($)
-- f4 = (<*>)

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 f ma = do
  a <- ma
  f a
-- f5 = (=<<)

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 f g x = do
  y <- f x
  g y 

-- forever ma  in Haskell   ~    while(true) { ma } 
forever :: Monad m => m a -> m b
forever ma = do
  ma
  forever ma

-- whileM cond ma        ~    while(cond) { ma }
whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond ma = do
  b <- cond
  if b 
    then do x <- ma
            xs <- whileM cond ma
            pure (x:xs)
    else pure []

-- forM xs f       ~    for(x in xs) { f x }
forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM = flip mapM

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM f e [] = pure e
foldrM f e (x:xs) = do
  y <- foldrM f e xs
  f x y
