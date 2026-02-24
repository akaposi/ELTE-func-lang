module Ea03 where

import Data.Monoid (Product(..), All(..))
import Data.List

-- typeclasses: semigroups, monoids, foldable

{-
class Semigroup (m :: Type) where
  (<>) :: m -> m -> m   -- append
-}
{-
instances need to satisfy:
associativity: (x <> y) <> z == x <> (y <> z)
-}

-- Haskell doesn't check instances satisfy equations

{-
instance Semigroup [a] where
  xs <> ys = xs ++ ys
-}

{-
class Semigroup m => Monoid m where
  mempty :: m

need to satisfy:
left unit (identity): mempty <> x = x
right unit: x <> mempty = x
-}

{-
instance Monoid [a] where
  mempty = []
-}

mconcat' :: Monoid a => [a] -> a
mconcat' [] = mempty
mconcat' (x:xs) = x <> mconcat' xs

-- instance Semigroup Int where
--   (<>) = (+)
-- -- (x + y) + z = x + (y + z)

-- instance Monoid Int where
--   mempty = 0

-- instance Semigroup Int where
--   (<>) = (*)

-- instance Monoid Int where
--   mempty = 1

-- newtype is almost the same as data
-- newtype only works with data with single constructor and single field
-- more efficient representation, memory representation same as Int
newtype Sum = Sum Int -- data Sum = Sum Int
  deriving (Show)

getSum :: Sum -> Int
getSum (Sum x) = x

instance Semigroup Sum where
  Sum x <> Sum y = Sum $ x + y

instance Monoid Sum where
  mempty = Sum 0

sum' :: [Int] -> Int
sum' xs = getSum $ mconcat (Sum <$> xs)

-- can do the same thing with: newtype Product = Product Int

-- Semigroup Bool, Monoid Bool
-- (||), False
-- (&&), True
-- xor, False    -- addition mod 2
-- (==), True

{-
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x1, y1) <> (x2, y2) = (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  (<>) :: (a -> b) -> (a -> b) -> (a -> b)
  f <> g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty
-}

-- power x 4 == x <> x <> x <> x
naivePower :: Monoid m => m -> Int -> m
naivePower x n
  | n <= 0 = mempty
  | otherwise = x <> naivePower x (n - 1)

power :: Monoid m => m -> Int -> m
power x n
  | n <= 0 = mempty
  | even n = let res = power x (n `div` 2) in res <> res
  | otherwise = x <> power x (n - 1)

-- naivePower x 4 = x <> (x <> (x <> (x <> mempty)))
-- power x 4      = (x <> x) <> (x <> x)

fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

{-
curr = 0
next = 1
for (1..n)
  curr = next
  next = curr + next
return curr
-}

fibBetter :: Integer -> Integer
fibBetter n = helper n 0 1
  where
    helper 0 curr next = curr
    helper n curr next = helper (n - 1) next (curr + next)

-- fibBetter 3
-- = helper 3 0 1
-- = helper 2 1 1
-- = helper 1 1 2
-- = helper 0 2 3
-- = 2

-- 2 x 2 matrices
newtype M22 = M22 [[Integer]]

getM22 :: M22 -> [[Integer]]
getM22 (M22 xs) = xs

fibMatrix :: M22
fibMatrix = M22 [[1, 1], [1, 0]]

instance Semigroup M22 where
  M22 a <> M22 b =
    M22 [[sum (zipWith (*) aRow bCol) | bCol <- transpose b] | aRow <- a]

{-
            [b00,               b01]
            [b10,               b11]
  [a00, a01](a00*b00 + a01*b10)
  [a10, a11]
-}

instance Monoid M22 where
  mempty = M22 [[1, 0], [0, 1]]

fibBest :: Integer -> Integer
fibBest n = getM22 (power fibMatrix (fromIntegral n)) !! 0 !! 1


{-
class Eq a where
  (==)
  (/=)
-}
{-
class Foldable (f :: Type -> Type) where
  foldMap :: Monoid m => (a -> m) -> f a -> m -- generalization of mconcat

  foldr :: (a -> b -> b) -> b -> f a -> b
  foldl :: (b -> a -> b) -> b -> f a -> b

  -- Foldable instance: either implement foldMap or foldr
-}

{-
instance Foldable [] where
  foldMap :: Monoid m => (a -> m) -> [a] -> m
  foldMap f [] = mempty
  foldMap f (x:xs) = f x <> foldMap f xs
-}

-- trees where elements are at the nodes
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

example :: Tree Int
example = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)

sum'' :: Foldable f => f Int -> Int
sum'' xs = getSum (foldMap Sum xs)

product'' :: Foldable f => f Int -> Int
product'' xs = getProduct (foldMap Product xs)

all' :: Foldable f => (a -> Bool) -> f a -> Bool
all' f xs = getAll (foldMap (All . f) xs)


foldMap_ :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap_ f t = foldr (\a m -> f a <> m) mempty t

newtype Endo b = Endo (b -> b) -- endofunction

getEndo :: Endo b -> b -> b
getEndo (Endo f) = f

instance Semigroup (Endo b) where
  Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo b) where
  mempty = Endo id

foldr_ :: Foldable f => (a -> b -> b) -> b -> f a -> b
foldr_ f b t = getEndo (foldMap (\a -> Endo (f a)) t) b

-- f x (f y (f z b)) == (f x . f y . f z $ b)

-- Endo' f <> Endo' g = Endo' (g . f)
newtype Dual a = Dual a

getDual :: Dual a -> a
getDual (Dual x) = x

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

-- m = Dual (Endo b)
foldl_ :: Foldable f => (b -> a -> b) -> b -> f a -> b
foldl_ f b t =
  getEndo (getDual (foldMap (\a -> Dual (Endo (flip f a))) t)) b

-- flip f z . flip f y . flip f x $ b
-- = f (f (f b x) y) z
