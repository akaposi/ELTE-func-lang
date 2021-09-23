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

data T a = T1 Int a [a]

instance Functor T where
  fmap f (T1 x y z) = T1 (fx x) (fy y) (fz z)
    where fx = id
          fy = f
          fz = map f

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)

-- Define without pattern matching, using only fmap:
-- mapNested (+1) [[0], [1, 2]] = [[1], [2, 3]]

mapNested' :: (a -> b) -> [[a]] -> [[b]]
mapNested' f = map (map f) 

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
  fmap f (Const x) = Const _
      -- f :: a -> b 
      -- x :: x
      -- _ :: x

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


-- Additional exercises:
 
returnList :: a -> [a]
returnList x = [x]

bindList :: (a -> [b]) -> [a] -> [b]
bindList f xs = [ y | x <- xs, y <- f x ]

concatList :: [[a]] -> [a]
concatList = concat

-- Define the following functions for Maybe:
---- An element of (Maybe a) can be seen as a list of 'a's with at most one element.

returnMaybe :: a -> Maybe a
returnMaybe = undefined

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe = undefined

-- Use bindMaybe to redefine concatMaybe
-- Also concatMaybe to redefine bindMaybe