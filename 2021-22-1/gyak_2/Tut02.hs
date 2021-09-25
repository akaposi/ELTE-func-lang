{-# OPTIONS -fwarn-incomplete-patterns #-} 
module Tut02 where
import Prelude hiding (Eq(..), Ord(..), Functor(..))

----- The Eq and Ord typeclasses:

-- ghci> :i Eq
class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  -- False == False = True
  -- True  == False = False
  -- False == True  = False
  -- True  == True  = True

  False == False = True
  True  == True  = True
  _     == _     = False

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = x == y
                    -- (Eq a)
  _       == _       = False

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  --                  (Eq a)    (Eq [a])
  _      == _      = False

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Right x == Right y = x == y
  _ == _ = False

instance (Eq a, Eq b) => Eq (a, b) where
  (x1, y1) == (x2, y2) = (x1 == x2) && (y1 == y2)

-- ghci> :i Ord
class Eq a => Ord a where
  (<=) :: a -> a -> Bool

instance Ord Bool where
  False <= False = True
  False <= _     = True
  _     <= False = False

  True  <= True  = True

instance Ord a => Ord (Maybe a) where
  Nothing <= Nothing = True
  Nothing <= _       = True
  _       <= Nothing = False

  Just x  <= Just y  = x <= y

instance Ord a => Ord [a] where
  [] <= [] = True
  [] <= _  = True
  _  <= [] = False

  (x:xs) <= (y:ys)
    | x == y        = xs <= ys
    | otherwise     = x <= y

-- [1, 2] <= [2, 1]

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

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)