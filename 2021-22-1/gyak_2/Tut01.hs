{-# OPTIONS -fwarn-incomplete-patterns #-} 
module Tut01 where
import Prelude hiding (Eq(..), Ord(..))

-- This file: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1/gyak_2/Tut01.hs

----- General information:
-- https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2021-22-1

----- Holes:
-- Reload in GHCI or press F8 in Visual Studio code to see the type of a hole `_`

ex0 :: a -> b -> a
ex0 x y = _

----- Common Algebraic Datatypes: (,), Either, Maybe

-- data Maybe a = Nothing 
--              | Just a

-- product types
-- data (,) a b = (,) a b
--      (a,b)

--     (x,y) :: (a,b)
--   when x :: a
--        y :: b

-- sum types
-- data Either a b = Left a
--                 | Right b

-- Left x :: Either a b
--   when x :: a
-- Right x :: Either a b
--   when x :: b

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show) 
 -- <-- this defines `show :: Show a => BinTree a -> String` automatically. 

data Tree1 a = Leaf1 a
             | Node1 [Tree1 a]
             deriving (Show)

----- Exercises: define functions f1 .. f11 with the given types.

-- fst :: (a,b) -> a
-- snd :: (a,b) -> b
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (x,(y,(z,w))) = (y , z)

f2 :: (a -> b) -> a -> b
f2 f x = f x
-- f2 f = f

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g x = f (g x)

f4    :: (a -> b -> c) -> (b -> a -> c)
-- f4 :: (a -> b -> c) -> b -> a -> c
f4 f x y = f y x

f5    :: ((a, b) -> c) -> (a -> b -> c)
-- f5 :: ((a, b) -> c) -> a -> b -> c
f5 f x y = f (x , y)
-- f5 = curry
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
-- f6 f = (h1 , h2)
--   where -- h1 :: a -> b
--         h1 x = fst (f x)
--             -- f x :: (b , c)
--             -- fst (f x) :: b
--         -- h2 :: a -> c
--         h2 x = snd (f x)

f6 f = (h1 , h2)
  where h1 = \x -> fst (f x)
        h2 = \x -> snd (f x)

-- f6 f = (\x -> fst (f x), \x -> snd (f x))

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f,g) x = (f x, g x)

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 g = (h1, h2)
  where h1 x = g (Left x)
        h2 x = g (Right x)
        -- Left  :: a -> Either a b
        -- Right :: b -> Either a b

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f1, f2) (Left x)  = f1 x
f9 (f1, f2) (Right y) = f2 y

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left (x,y)) = (x, Left y)
f10 (Right (x,y)) = (x, Right y)

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (x, Left y) = Left (x,y)
f11 (x, Right y) = Right (x,y)
