{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}

module Notes02 where

data Color = Red 
           | Blue
           | Green
           deriving (Show, Eq)

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show, Eq)

-- data Ordering = LT | EQ | GT
class Eq a => Ord' a where
  compare' :: a -> a -> Ordering

-- (<) :: Ord a => a -> a -> Bool   in Prelude
lt :: Ord' a => a -> a -> Bool
lt x y = compare' x y /= GT

-- (<=) :: Ord a => a -> a -> Bool   in Prelude
lte :: Ord' a => a -> a -> Bool
lte x y = compare' x y /= GT

--------------------------------------------------------------------------------
-- Define the following Ord' instances.

instance Ord' Color where
  compare' :: Color -> Color -> Ordering
  compare' Red   Red   = EQ
  compare' Red   _     = LT
  compare' _     Red   = GT

  compare' Blue  Blue  = EQ
  compare' Blue  _     = LT
  compare' _     Blue  = GT

  compare' Green Green = EQ

instance (Ord' a, Ord' b) => Ord' (a, b) where
  compare' = undefined

instance (Ord' a, Ord' b) => Ord' (Either a b) where
  compare' = undefined

instance Ord' a => Ord' [a] where
  compare' = undefined

instance Ord' a => Ord' (BinTree a) where
  compare' = undefined

--------------------------------------------------------------------------------
-- You know the function map on lists:

map' :: (a -> b) -> [a] -> [b]
map' f xs = undefined

-- Define mapping functions for other types:
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair = undefined

mapEither :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapEither = undefined

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree = undefined

