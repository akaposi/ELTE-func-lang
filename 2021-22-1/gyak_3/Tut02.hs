{-# GHC_OPTIONS -fwarn-incomplete-patterns #-} 
module Tut02 where
import Prelude hiding (Eq(..), Ord(..), Functor(..))

----- The Eq and Ord typeclasses:

-- ghci> :i Eq
class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  (==) = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Eq a => Eq [a] where
  (==) = undefined

instance (Eq a, Eq b) => Eq (Either a b) where
  (==) = undefined

instance (Eq a, Eq b) => Eq (a, b) where
  (==) = undefined

-- ghci> :i Ord
class Ord a where
  (<=) :: a -> a -> Bool

instance Ord Bool where
  (<=) = undefined

instance Ord a => Ord (Maybe a) where
  (<=) = undefined


----- The Functor typeclass:


-- ghci> :i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = undefined

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap = undefined

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
