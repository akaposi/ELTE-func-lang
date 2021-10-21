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
modify f = do s <- get; put (f s)

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
impFactorial n = do
  -- put 1
  forM_ [1..n] $ \i -> do
    modify (\x -> x * i)

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    (a, b) := (1, 1)
--    for i from 1 to n
--      (a, b) := (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = do
  -- put (1,1)
  forM_ [1..n] $ \_ -> do
    -- (a,b) <- get
    -- put (b, a+b)
    modify $ \(a,b) -> (b, a+b)

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))

-- impGcd should be a translation of the imperative program 
--   (a, b) are inputs
--   while b /= 0
--     (a, b) := (b, a `mod` b)

impGcd :: State (Integer, Integer) ()
impGcd = whileM_ 
           (do (a,b) <- get 
               return (b /= 0) -- b /= 0
           )
           (do modify $ \(a,b) -> (b, a `mod` b)) -- (a, b) := (b, a `mod` b)

runGcd :: Integer -> Integer -> Integer
runGcd x y = fst $ execState impGcd (x, y)

--- Foldable

-- class Monoid m where
--   mempty :: m
--   (<>)   :: m -> m -> m

--   mempty <> x = x = x <> mempty     -- mempty is neutral for (<>)
--   (x <> y) <> z == x <> (y <> z)    -- (<>) is associative

class Functor f => Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m

foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b
foldr f e t = foldMap (Endo . f) t `appEndo` e

-- There is no: "instance Foldable ((->) x)"

-- fmap f []     = []
-- fmap f (x:xs) = (:) (f x) (fmap f xs)

instance Foldable [] where
  foldMap f []     = mempty
  foldMap f (x:xs) = f x <> foldMap f xs
                -- = mconcat [f x, foldMap f xs]

null :: Foldable f => f a -> Bool
null = foldr (\_ _ -> False) True

toList :: Foldable f => f a -> [a]
-- toList = foldr (\x xs -> x:xs) []
toList = foldr (:) []

length' :: Foldable f => f a -> Int
length' = foldr (\_ l -> l+1) 0

sum' :: (Foldable f, Num a) => f a -> a
sum' = foldr (\x s -> x + s) 0

instance Foldable Maybe where
  foldMap f Nothing  = mempty
  foldMap f (Just x) = f x

instance Foldable (Either x) where
  foldMap f (Left c)  = mempty
  foldMap f (Right b) = f b

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

data Tree2 a = Leaf2 a | Node2 [Tree2 a]
  deriving (Functor)

-- fmap f (Node2 xs) = Node2 (fmap (fmap f) xs)

instance Foldable Tree2 where
  foldMap f (Leaf2 x)  = f x
  foldMap f (Node2 xs) = foldMap (foldMap f) xs

