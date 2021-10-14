{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Tut06 where

import Prelude hiding (Foldable(..))
import Control.Monad
import Data.Monoid

-- The State monad

data State s a = State { runState :: s -> (a, s) }
  deriving (Functor)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

-- Basic operations:

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

-- Examples: 
incr :: State Int ()
incr = modify (+1)

push :: a -> State [a] ()
push x = modify (\xs -> x:xs)

pop :: State [a] (Maybe a)
pop = do
  xs <- get
  case xs of
    y:ys -> do put ys; pure (Just y)
    []   -> pure Nothing

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

-- forM :: Monad m => [a] -> (a -> m b) -> m [b]
-- forM_ :: Monad m => [a] -> (a -> m b) -> m () -- forM_ discards the results of forM

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond ma = do
  b <- cond
  if b then (:) <$> ma <*> whileM cond ma
       else pure []

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond ma = whileM cond ma >> pure ()

--- Simple imperative programs in the State monad

-- Example:
--   x := 1
--   for i from 1 to n
--     x := x + 1
ex :: Integer -> State Integer ()
ex n = do
  put 1                   -- x := 1
  forM_ [1..n] $ \_ -> do -- for i from 1 to n
    modify (\x -> x + 1)  --   x := x+1

runEx :: Integer -> Integer
runEx n = execState (ex n) 1

-- impFactorial should be a translation of the imperative program
--    x := 1
--    for i from 1 to n
--      x := x * i

impFactorial :: Integer -> State Integer ()
impFactorial n = undefined

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    (a, b) := (1, 1)
--    for i from 1 to n
--      (a, b) := (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = undefined

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))

-- impGcd should be a translation of the imperative program 
--   (a, b) are inputs
--   while b /= 0
--     (a, b) := (b, a `mod` b)

impGcd :: State (Integer, Integer) ()
impGcd = undefined

runGcd :: Integer -> Integer -> Integer
runGcd x y = fst $ execState impGcd (x, y)

--- Foldable

class Functor f => Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m

foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b
foldr f e t = foldMap (Endo . f) t `appEndo` e

instance Foldable [] where
  foldMap f []     = mempty
  foldMap f (x:xs) = f x <> foldMap f xs
                -- = mconcat [f x, foldMap f xs]

null :: Foldable f => f a -> Bool
null = undefined

length' :: Foldable f => f a -> Int
length' = undefined

sum' :: (Foldable f, Num a) => f a -> a
sum' = undefined

instance Foldable Maybe where
  foldMap = undefined

instance Foldable (Either x) where
  foldMap = undefined

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor)

instance Foldable Tree where
  foldMap = undefined

data Tree2 a = Leaf2 a | Node2 [Tree2 a]
  deriving (Functor)

instance Foldable Tree2 where
  foldMap = undefined
