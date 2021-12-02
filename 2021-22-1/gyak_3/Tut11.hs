{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

--------------------------------------------------------------------------------

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Define Functor, Foldable and Traversable instances for the following types:

data AndOr a b = Left a 
               | Right b
               | Both a b
               deriving (Eq, Ord, Show)

data Tree1 a = LeafA1 Int a
             | LeafB1 a a
             | Node1 [Tree1 a]
             deriving (Eq, Ord, Show)

data Tree21 a = Leaf21 a 
              | Node21 (Maybe (Tree22 a))
              deriving (Eq, Ord, Show)

data Tree22 a = Leaf22 a 
              | Node22 [Tree21 a]
              deriving (Eq, Ord, Show)

-- Bonus:
data Tree3 a = Leaf3 a 
             | Node2 [Tree3 (a,a)]
             deriving (Eq, Ord, Show)

exTree3 :: Tree3 Int
exTree3 = Node2 [ Leaf3 (0,1), Node2 [ Leaf3 ((0,1),(2,3)) ] ]

--------------------------------------------------------------------------------
-- https://github.com/AndrasKovacs/ELTE-func-lang/blob/master/2019-20-2/vizsga_minta/minta4/Feladatok.md

data Tree a = Leaf1 a 
            | Leaf2 a a 
            | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)

ex1 :: Tree Int
ex1 =
  Node (Leaf2 2 1)
       (Just (Node (Leaf1 10)
                   (Just (Node (Leaf2 5 6)
                               Nothing))))

-- `leftmost t` returns the value of the leftmost leaf of the tree `t`.
leftmost :: Tree a -> a
leftmost = undefined

-- `findInLeaf2 p t` returns the first element of `t` that is inside a `Leaf2` 
--   and satisfies the predicate `p`, if such an element exists.
findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
findInLeaf2 = undefined

-- `countLeaf1s t` should label every value of `t` with the number 
--   of `Leaf1` constructors to its left.
countLeaf1s :: Tree a -> Tree (a, Int)
countLeaf1s = undefined

-- `replaceLeaf2s xs t` should replace the elements of `t` that are 
--    part of a `Leaf2` constructor by elements from the list `xs`.
replaceLeaf2s :: [a] -> Tree a -> Tree a
replaceLeaf2s = undefined