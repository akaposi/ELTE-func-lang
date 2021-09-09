{-# GHC_OPTIONS -fwarn-incomplete-patterns #-} 
module Tut01 where
import Prelude hiding (Eq(..), Ord(..))

-- This file: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1/gyak_2/Notes01.hs

----- General information:
-- https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1

----- Holes:

ex0 :: a -> b -> a
ex0 = _

----- Common Algebraic Datatypes: (,), Either, Maybe

-- data Maybe a = Nothing 
--              | Just a

-- data (,) a b = (,) a b

-- data Either a b = Left a
--                 | Right b

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show) -- <-- this defines `show :: Show a => BinTree a -> String` automatically. 

data Tree1 a = Leaf1 a
             | Node1 [Tree1 a]
             deriving (Show)

----- Exercises: define functions f1 .. f11 with the given types.

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

----- The Eq and Ord typeclasses:

-- ghci> :i Eq
class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  (==) = undefined

instance Eq a => Eq (Maybe a) where
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
