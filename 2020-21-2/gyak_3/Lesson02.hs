{-# options_ghc -Wincomplete-patterns #-}

module Lesson02 where

-- Define the following functions in a type correct and total manner.
-- (No infinite loops or exceptions allowed.)

f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = undefined

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 = undefined

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 = undefined

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 = undefined

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 = undefined

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 = undefined

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 = undefined

-- Extra (harder) task:
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 = undefined


{- Algebraic Data Types (ADT) -}

data Color = Red | Green | Blue

data List a = Nil | Cons a (List a)
data Pair a b = MakePair a b
data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node l r) = 1 + (max (height l) (height r))

treeSum :: Tree Int -> Int
treeSum t = undefined


{- Type Class Instances -}

instance Show Color where
  show Red = "r"
  show Green = "g"
  show Blue = "b"

instance Show a => Show (List a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ ", " ++ (show xs)

instance Show (Pair a b) where
  show t = undefined

instance Show (Tree a) where
  show t = undefined

instance Eq a => Eq (Tree a) where
  t == t' = undefined

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

instance Ord Color where
  Red   <= _     = True
  Green <= Green = True
  _     <= Blue  = True
  _     <= _     = False

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool

instance Eq a => Ord (Tree a) where -- `Eq (Tree a)` is included!
  t <= t' = undefined

mapPair :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
mapPair pac = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe ma = undefined

mapList :: (a -> b) -> List a -> List b
mapList l = undefined

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = undefined

instance Functor Tree where
  fmap = undefined
