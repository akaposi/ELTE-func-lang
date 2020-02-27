{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS -Wincomplete-patterns #-}
module Practice2 where

data List a = Nil
            | Cons a (List a)
            deriving (Show)

instance Eq a => Eq (List a) where
  Nil       == Nil       = True
  Cons x xs == Cons y ys = (x == y) && (xs == ys)
  _         == _         = False

data Either' a b = Left' a
                 | Right' b
                 deriving (Show)

-- Define an Eq instance for Either'
instance (Eq a, Eq b) => Eq (Either' a b) where
 (==) :: (Eq a1, Eq a2) => Either' a1 a2 -> Either' a1 a2 -> Bool
 Left' x  == Left' y  = (x == y)
 Right' x == Right' y = (x == y)
 _        == _        = False

data Pair a b = Pair a b
              deriving (Show)

instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair xa xb == Pair ya yb = (xa == ya) && (xb == yb)

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show)

instance Eq a => Eq (BinTree a) where
  Leaf x     == Leaf y     = x == y
  Node xl xr == Node yl yr = (xl == yl) && (xr == yr)
  _          == _          = False

-- class Semigroup a where
--   (<>) :: a -> a -> a
--      associativity: (x <> y) <> z == x <> (y <> z)
-- class Semigroup a => Monoid a where
--   mempty :: a
--      mempty <> x == x
--      x <> mempty == x

instance Semigroup (List a) where
  -- List concatenation
  Nil         <> ys = ys
  (Cons x xs) <> ys = x `Cons` (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

-- Define mconcat' and concatTree
mconcat' :: Monoid a => [a] -> a
mconcat' = foldr (<>) mempty
-- mconcat' ["abc", "def", "ghi"] = "abcdefghi"

concatTree :: Semigroup a => BinTree a -> a
concatTree (Leaf x)   = x
concatTree (Node l r) = concatTree l <> concatTree r
-- concatTree (Node (Leaf "abc") (Leaf "def")) = "abcdef"
