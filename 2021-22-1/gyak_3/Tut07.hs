{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable #-}
module Tut07 where

import Prelude hiding (Traversable(..))
import Control.Monad
import Data.Monoid

data State s a = State { runState :: s -> (a, s) }
  deriving (Functor)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do s <- get; put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Foldable)

-- Define the function `labelTree :: Tree a -> Tree Int`.
--   `labelTree t` should label the leaves by the integers 0,1,...
-- Examples:
--   labelTree (Leaf ()) == Leaf 0
--   labelTree (Node (Leaf ()) (Leaf ())) 
--     == Node (Leaf 0) (Leaf 1)
--   labelTree (Node (Node (Leaf ()) (Leaf ())) (Leaf ()))
--     == Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

labelTree :: Tree a -> Tree Int
labelTree t = evalState (go t) 0
  where go :: Tree a -> State Int (Tree Int)
        go (Leaf x)   = do
          a <- get
          put (a+1)
          pure (Leaf a)
        go (Node l r) = Node <$> go l <*> go r
        -- go (Node l r) = do
        --   -- state: a
        --   l' <- go l
        --   -- state: a + length l
        --   r' <- go r
        --   -- state: a + length l + length r
        --   pure (Node l' r')
        -- go (Node l r) = liftM2 Node (go l) (go r)

-- `relabelTree xs t` should label the leaves using the values of xs.
--  (You can assume that length xs >= length t)
-- Examples:
--   relabelTree [10] (Leaf ()) == Leaf 10
--   relabelTree [2,1] (Node (Leaf ()) (Leaf ())) 
--     == Node (Leaf 2) (Leaf 1)
--   relabelTree [9,2,7] (Node (Node (Leaf ()) (Leaf ()) (Leaf ()))
--     == Node (Node (Leaf 9) (Leaf 2)) (Leaf 7)

relabelTree :: [b] -> Tree a -> Tree b
relabelTree xs t = evalState (go t) xs
  where go :: Tree a -> State [b] (Tree b)
        go (Leaf x)   = do
          ys <- get
          let z:zs = ys
          put zs
          pure (Leaf z)
        go (Node l r) = Node <$> go l <*> go r

--

-- In Prelude:
--   lookup :: Eq a => a -> [(a,b)]  -> Maybe b
--   lookup "key1" [("key1", 0), ("key2", 1)] == Just 0
--   lookup "key3" [("key1", 0), ("key2", 1)] == Nothing

-- Examples:
--   mapLookup [("a", 0), ("b", 1)] (Node (Leaf "b") (Leaf "a"))
--     == Just (Node (Leaf 1) (Leaf 0))
--   mapLookup [("a", 0), ("b", 1)] (Node (Leaf "a") (Leaf "c"))
--     == Nothing

-- `mapLookup xs t` should apply the function `swap lookup xs` to the
--   values at the leaves of `t`, and fail (return Nothing)
--   if any of the lookups fails.

mapLookup :: Eq a => [(a,b)] -> Tree a -> Maybe (Tree b)
mapLookup xs (Leaf a)   = do
  b <- lookup a xs
  pure (Leaf b)
mapLookup xs (Node l r) = Node <$> mapLookup xs l <*> mapLookup xs r

--------------------------------------------------------------------------------

class Foldable f => Traversable f where
  -- fmap    ::                (a ->   b) -> f a ->    f b
  -- foldMap :: Monoid m =>    (a -> m)   -> f a -> m
  traverse :: Applicative m => (a -> m b) -> f a -> m (f b)

instance Traversable [] where
  traverse f []     = pure []
  traverse f (x:xs) = (:) <$> f x <*> traverse f xs

relabel' :: Traversable f => [b] -> f a -> f b
relabel' xs t = evalState (go t) xs
  where go :: Traversable f => f a -> State [b] (f b)
        go = traverse $ \x -> do
               ys <- get
               let z:zs = ys
               put zs
               pure z

mapLookup' :: (Eq a, Traversable f) => [(a,b)] -> f a -> Maybe (f b)
mapLookup' xs = traverse $ \a -> lookup a xs
-- mapLookup' xs = traverse (swap lookup xs)

instance Traversable Maybe where
  traverse f Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x

instance Traversable (Either x) where
  traverse = undefined

instance Traversable Tree where
  traverse f (Leaf x)   = Leaf <$> f x
  traverse f (Node x y) = Node <$> traverse f x <*> traverse f y

data Tree2 a = Leaf2 a 
             | Node2 [Tree2 a]
  deriving (Functor, Foldable)

-- fmap f (Node2 xs) = Node2 (fmap (fmap f) xs)
instance Traversable Tree2 where
  traverse f (Leaf2 x)  = Leaf2 <$> f x
  traverse f (Node2 xs) = Node2 <$> traverse (traverse f) xs


-- Bonus (fmapDefault and foldMapDefault in Data.Traversable):
fmapFromTraverse :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
fmapFromTraverse = undefined

foldMapFromTraverse :: Traversable f => (a -> b) -> f a -> f b
foldMapFromTraverse = undefined
