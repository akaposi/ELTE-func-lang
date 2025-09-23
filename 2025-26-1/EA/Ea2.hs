module Ea2 where

import Data.Monoid
import Data.List

-- Eq, Ord
-- ==, <

-- Semigroup (félcsoport), Monoid

{-
class Semigroup a where
  (<>) :: a -> a -> a
-}

-- String is semigroup
-- List is semigroup

-- Semigroup law:
-- associativity: (x <> y) <> z == x <> (y <> z)

-- ([1, 2, 3] <> [4, 5, 6]) <> [7, 8, 9] == [1, 2, 3] <> ([4, 5, 6] <> [7, 8, 9])
-- [1, 2, 3] <> [4, 5, 6] <> [7, 8, 9]
-- [1, 2, 3] ++ [4, 5, 6] ++ [7, 8, 9]

-- 1 + 2 + 3 == (1 + 2) + 3 == 1 + (2 + 3)

{-
class Semigroup a => Monoid a where
  mempty :: a
-}

-- Monoid laws:
-- mempty <> x == x
-- x <> mempty == x

-- identity element, unit element

-- (mempty :: [a]) == []

mconcat' :: Monoid m => [m] -> m
mconcat' xs = foldr (<>) mempty xs

{-
instance Semigroup Int where
  x <> y = x + y

instance Monoid Int where
  mempty = 0

instance Semigroup Int where
  x <> y = x * y
-}

{-
newtype Sum = Sum {getSum :: Int}
  deriving (Show)
-- more efficient, same memory representation as Int
-- only works if we have one constructor with one field

data Sum' = Sum' Int

instance Semigroup Sum where
  Sum x <> Sum y = Sum (x + y)

instance Monoid Sum where
  mempty = Sum 0

-- sum

newtype Prod = Prod {getProd :: Int}

instance Semigroup Prod where
  Prod x <> Prod y = Prod (x * y)

instance Monoid Prod where
  mempty = Prod 1
-}

-- (-), (1 - 2) - 3 /= 1 - (2 - 3)

-- (&&)
-- (||)

equiv :: Bool -> Bool -> Bool
equiv True True = True
equiv False False = True
equiv _ _ = False

newtype Equiv = Equiv Bool
-- mempty = Equiv True

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False
-- addition mod 2

-- Booleans have 4 monoid instances

{-
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a, b) <> (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
-}

{-
instance Semigroup b => Semigroup (a -> b) where
  f <> g = \x -> f x <> g x
  -- f :: a -> b
  -- g :: a -> b
  -- _ :: a -> b

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty
  -- a -> b
-}

-- power :: Monoid m => m -> Integer -> m
-- power x n
--   | n == 0 = mempty
--   | otherwise = x <> power x (n - 1)
-- O(n)
-- O(log(n))

-- power x 4 = (x <> (x <> (x <> (x <> mempty))))

power :: Monoid m => m -> Integer -> m
power x 0 = mempty
power x 1 = x
power x n
  | even n = half <> half
  | otherwise = x <> power x (n - 1)
  where
    half = power x (n `div` 2)

-- power x 4 == ((x <> x) <> (x <> x))

-- power [1, 2] 3 == [1, 2, 1, 2, 1, 2]

fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

fibBetter :: Integer -> Integer
fibBetter n = go 0 1 n
  where
    go curr next n
      | n == 0 = curr
      | otherwise = go next (curr + next) (n - 1)

newtype M22 = M22 {getMatrix :: [[Integer]]}
  deriving (Show)

fibMatrix :: M22
fibMatrix = M22 [[1, 1], [1, 0]]

instance Semigroup M22 where
  M22 a <> M22 b =
    M22 [[sum $ zipWith (*) aRow bCol | bCol <- transpose b] | aRow <- a]

  --            [b11, b12]
  --            [b21, b22]
  -- [a11, a12]
  -- [a21, a22]

instance Monoid M22 where
  mempty = M22 [[1, 0], [0, 1]]

fibBest :: Integer -> Integer
fibBest n = getMatrix (power fibMatrix n) !! 0 !! 1

class Magma a where
  (><) :: a -> a -> a
-- not useful


-- Functor :: (Type -> Type) -> Contraint
{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

-- map :: (a -> b) -> [a] -> [b]

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)
-- instance Functor Maybe

-- bad map
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = map' f xs ++ [f x]

-- Functor laws:
-- fmap id x == x
-- fmap f (fmap g x) == fmap (f . g) x  -- (map fusion)
