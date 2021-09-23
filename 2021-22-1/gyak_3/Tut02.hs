{-# OPTIONS -fwarn-incomplete-patterns #-} 
module Tut02 where
import Prelude hiding (Eq(..), Ord(..), Functor(..))

----- The Eq and Ord typeclasses:

-- ghci> :i Eq
class Eq a where
  (==) :: a -> a -> Bool

instance Eq Bool where
  -- False == False = True
  -- False == True  = False
  -- True  == True  = False
  -- True  == True  = True

  False == False = True
  True  == True  = True
  _     == _     = False

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = x == y
  _       == _       = False

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  --                  (Eq a)      (Eq [a])
  _      == _      = False

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Right x == Right y = x == y
  _ == _ = False

instance (Eq a, Eq b) => Eq (a, b) where
  (x,y) == (x',y') = x==x'&&y==y'

-- ghci> :i Ord
class Eq a => Ord a where
  (<=) :: a -> a -> Bool

instance Ord Bool where
  -- False <= False = True
  -- False <= True  = True
  -- True  <= False = False
  -- True  <= True  = True

  False <= False = True
  False <= _     = True -- compare False with every constructor we have not handled yet
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
    | x == y       = xs <= ys
    | otherwise    = x <= y

  -- compare :: Ord a => a -> a -> Ordering
  --   instead of (<=)
  -- more efficient

-- [1,2] <= [2,1]

-- forall a, b, c.      either (a<=b)
                           --  (a == b)
                           --  (b <= a)

----- The Functor typeclass:

-- ghci> :i Functor

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- data T a = T1 (...)
--            T2 (...)

-- fmap :: (a -> b) -> (T a -> T b)
-- fmap f (T1 x y z) = T1 (fx x) (fy y) (fz z)
--    fx = id      (x :: Int)
--    fx = f       (x :: a)
--    fx = map f   (x :: [a])
-- fmap f (T2 x y) = T2 ...

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map'

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just x) = Just (f x)
