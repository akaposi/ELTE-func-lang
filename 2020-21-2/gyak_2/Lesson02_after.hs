{-# options_ghc -Wincomplete-patterns #-}

module Lesson02 where

-- Define the following functions in a type correct and total manner.
-- (No infinite loops or exceptions allowed.)

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> a -> b
f2 f a = f a

f3 :: (b -> c) -> (a -> b) -> (a -> c)
f3 f g a = f (g a)

f4 :: (a -> (b -> c)) -> b -> a -> c
f4 f b a = f a b

f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b)

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 f = (\a -> case (f a) of { (b, c) -> b }, \a -> (snd (f a)))

f7 :: (a -> b, a -> c) -> a -> (b, c)
f7 (f, g) a = (f a, g a)

-- data Either a b = Left a | Right b

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (\a -> (f (Left a)), \b -> (f (Right b)))

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f, g) (Left a) = f a
f9 (f, g) (Right b) = g b

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
treeSum (Leaf x) = x
treeSum (Node l r) = (treeSum l) + (treeSum r)

{- Type Class Instances -}

instance Show Color where
  show Red = "r"
  show Green = "g"
  show Blue = "b"

instance Show a => Show (List a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ ", " ++ (show xs)

instance (Show a, Show b) => Show (Pair a b) where
  show (MakePair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

-- [[(5) + (3)] + (8)]
--     ·
--    / \
--   ·   8
--  / \
-- 5   3
instance Show a => Show (Tree a) where
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Node l r) = "[" ++ (show l) ++ " + " ++ (show r) ++ "]"
-- show :: a -> String (bound because of type constraint)

instance Eq a => Eq (Tree a) where
  -- (==) :: (Tree a) -> (Tree a) -> Bool
  (Leaf x) == (Leaf x') = x == x'
  (Node l r) == (Node l' r') = (l == l') && (r == r')
  _ == _ = False
-- (==) :: a -> a -> Bool (bound because of type constraint)

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

instance Eq a => Ord (Tree a) where
  t <= t' = height t <= height t'

mapPair :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
mapPair f g (MakePair a c) = MakePair (f a) (g c)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just a) = Just (f a)
mapMaybe f Nothing = Nothing

mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = mapList

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
