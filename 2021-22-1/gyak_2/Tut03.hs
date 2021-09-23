{-# OPTIONS -fwarn-incomplete-patterns #-} 
module Tut03 where
import Prelude hiding (Functor(..))

----- The Functor typeclass:
-- ghci> :i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map' 

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- data T a = T1 .....
--          | T2 ...
-- 
-- fmap :: (a -> b) -> T a -> T b
-- fmap f (T1 ...) = T1 ...
-- fmap f (T2 ...) = T2 ...

-- fmap f (T1 x y z) = T1 (fx x) (fy y) (fz z)
--   fx = id     (if x :: Int)
--   fx = f      (if x :: a)
--   fx = map f  (if x :: [a])

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

-- Define without pattern matching, using only fmap:
mapNested :: (a -> b) -> [[[a]]] -> [[[b]]]
mapNested = undefined

data Id a = Id a
          deriving (Show)

instance Functor Id where
  -- fmap :: (a -> b) -> Id a -> Id b
  fmap = undefined

data Const a b = Const a
               deriving (Show)

instance Functor (Const a) where
  -- fmap :: (a -> b) -> Const x a -> Const x b
  fmap = undefined

data BinTree a = BinLeaf a
               | BinNode (BinTree a) (BinTree a)
               deriving (Show)

instance Functor BinTree where
  -- fmap :: (a -> b) -> BinTree a -> BinTree b
  fmap = undefined

data Tree1 a = Leaf1 a
             | Node1 [Tree1 a]
             deriving (Show)

instance Functor Tree1 where
  -- fmap :: (a -> b) -> Tree1 a -> Tree1 b
  fmap = undefined

data Tree2 a = Leaf2 a
             | Node2 (Int -> Tree2 a)
-- Remark: it is not possible to define Show, Eq or Ord for `Tree2`

instance Functor Tree2 where
  -- fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap = undefined