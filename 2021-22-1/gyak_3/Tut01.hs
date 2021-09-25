{-# OPTIONS -fwarn-incomplete-patterns #-} 
module Tut01 where
import Prelude hiding (Eq(..), Ord(..))

-- This file: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1/gyak_3/Tut01.hs

----- General information:
-- https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1

----- Holes:
-- Reload in GHCI or press F8 in Visual Studio code to see the type of a hole `_`

ex0 :: a -> b -> a
ex0 x y = _

----- Common Algebraic Datatypes: (,), Either, Maybe

-- data Maybe a = Nothing 
--              | Just a

-- data (,) a b = (,) a b

-- data Either a b = Left a
--                 | Right b

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show) 
      -- <-- this defines `show :: Show a => BinTree a -> String` automatically. 

data Tree1 a = Leaf1 a
             | Node1 [Tree1 a]
             deriving (Show)

----- Exercises: define functions f1 .. f11 with the given types.

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (x,(y,(z,w))) = (y , z)

f2 :: (a -> b) -> a -> b
f2 f y = f y

f3 :: (b -> c) -> (a -> b) -> a -> c
-- f3 g h x = g (h x)
f3 g h x = let y = h x 
           in g y
  -- x       :: a
  -- h x     :: b   (because h :: a -> b)
  -- g (h x) :: c   (because g :: b -> c)

  -- ? g (h x) :: c 

f4 :: (a -> b -> c) -> (b -> a -> c)
-- f4 :: (a -> b -> c) -> b -> a -> c
f4 g x y = g y x

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 f x y = f (x,y)

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
-- f6 f = (h1, h2)
--   where h1 x = fst (f x)
--         h2 x = snd (f x)
f6 f = (\x -> fst (f x), \x -> snd (f x))
        -- x :: a
        -- f x :: (b, c)
        -- fst (f x) :: b
        -- snd (f x) :: c

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f,g) a = (f a, g a)

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (\x -> f (Left x), \x -> f (Right x))

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f,g) (Left x) = f x
f9 (f,g) (Right x) = g x

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left (x,y)) = (x, Left y)
f10 (Right (x,y)) = (x, Right y)

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (x, Left y) = Left (x,y)
f11 (x, Right y) = Right (x,y)
