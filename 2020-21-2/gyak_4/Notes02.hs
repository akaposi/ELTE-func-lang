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

instance Ord' Integer where compare' = compare
instance Ord' Int where compare' = compare
instance Ord' Char where compare' = compare

lexi :: Ordering -> Ordering -> Ordering
lexi EQ y = y
lexi x  y = x

-- This is the same as (<>) :: Ordering -> Ordering -> Ordering in the Prelude.


-- Lexicographic order
instance (Ord' a, Ord' b) => Ord' (a, b) where
  compare' :: (a, b) -> (a, b) -> Ordering
  -- compare' (a1, b1) (a2, b2) = case compare' a1 a2 of
  --   EQ -> compare' b1 b2
  --   x  -> x
  compare' (a1, b1) (a2, b2) = compare' a1 a2 `lexi` compare' b1 b2

instance (Ord' a, Ord' b) => Ord' (Either a b) where
  compare' :: Either a b -> Either a b -> Ordering
  compare' (Left x) (Left y) = compare' x y
  compare' (Left _) _        = LT
  compare' _        (Left _) = GT

  compare' (Right x) (Right y) = compare' x y

instance Ord' a => Ord' [a] where
  compare' :: [a] -> [a] -> Ordering
  compare' [] [] = EQ
  compare' [] _  = LT
  compare' _  [] = GT

  compare' (x : xs) (y : ys) = compare' x y `lexi` compare' xs ys

instance Ord' a => Ord' (BinTree a) where
  compare' :: BinTree a -> BinTree a -> Ordering
  compare' (Leaf x) (Leaf y) = compare' x y
  compare' (Leaf _) _        = LT
  compare' _        (Leaf _) = GT

  compare' (Node l1 r1) (Node l2 r2) = compare' l1 l2 `lexi` compare' r1 r2

--------------------------------------------------------------------------------
-- You know the function map on lists:

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- Define mapping functions for other types:
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (a, c) = (f a, g c)

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f g (Left a)  = Left (f a)
mapEither f g (Right c) = Right (g c)

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f (Leaf x)   = Leaf (f x)
mapBinTree f (Node l r) = Node (mapBinTree f l) (mapBinTree f r)