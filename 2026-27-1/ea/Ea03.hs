module Ea03 where

import Data.Monoid (Sum(..), Product(..), All(..), Any(..))
import Data.List

-- f :: Type -> Type
-- Functor f

-- Eq a
{-
class Eq a where
  (==) :: a -> a -> Bool

-- type class laws

Eq laws:
reflexivity: (x == x) = True
symmetry: (x == y) == True ==> (y == x) == True
transitivity: (x == y) == True ==> (y == z) == True ==> (x == z) == True

(==) is an equivalence relation
-}

{-
class Semigroup a where
  (<>) :: a -> a -> a

semigroup law:
associativity: (x <> y) <> z == x <> (y <> z)

class Semigroup a => Monoid a where
  mempty :: a

monoid laws:
left unit: mempty <> x == x
right unit: x <> mempty = x
-}

{-
instance Semigroup [a] where
  x <> y = x ++ y

instance Monoid [a] where
  mempty = []
-}

-- Int (+)
-- Int (*)

-- data Sum = Sum Int
--   deriving (Show)

-- getSum :: Sum -> Int
-- getSum (Sum x) = x

-- instance Semigroup Sum where
--   Sum x <> Sum y = Sum (x + y)

-- instance Monoid Sum where
--   mempty = Sum 0

-- -- mconcat
-- mconcat' :: Monoid m => [m] -> m
-- mconcat' [] = mempty
-- mconcat' (x:xs) = x <> mconcat' xs

sum' :: [Int] -> Int
sum' xs = getSum $ mconcat (Sum <$> xs)

product' :: [Int] -> Int
product' xs = getProduct $ mconcat (Product <$> xs)



{-
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a1, b1) <> (a2, b2) = (a1 <> a2, b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
-}


{-
instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> Nothing = Nothing
  Nothing <> Just y = Just y
  Just x <> Nothing = Just x
  Just x <> Just y = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
-}


data First a = First a

instance Semigroup (First a) where
  First x <> First _ = First x

getFirst :: First a -> a
getFirst (First a) = a



{-
instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

  f :: a -> b
  g :: a -> b
  f <> g :: a -> b
-}




{-
monoid instances of Bool:
- (&&), True
- (||), False
- xor, False
- (==), True
-}
xor :: Bool -> Bool -> Bool
xor False False = False
xor True False = True
xor False True = True
xor True True = False

-- fun xs = getXor $ mconcat $ map Xor xs




{-
-- type synonym
type Sum = Int

-- pointer indirection
data Sum = Sum Int

-- more efficient, in memory it is just Int
-- only allowed when the type has a single constructor with single field
newtype Sum = Sum Int
-}





power' :: Monoid m => m -> Integer -> m
power' m n
  | n <= 0 = mempty
  | otherwise = m <> power' m (n - 1)

-- power' m 4 = m <> (m <> (m <> (m <> mempty)))
-- let m' = m <> m in m' <> m'

power :: Monoid m => m -> Integer -> m
power m n
  | n <= 0 = mempty
  | even n = let m' = power m (n `div` 2) in m' <> m'
  | otherwise = m <> power m (n - 1)





fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)




fibBetter :: Integer -> Integer
fibBetter n = helper 0 1 n
  where
    helper curr next n
      | n <= 0 = curr
      | otherwise = helper next (curr + next) (n - 1)






data M22 = M22 [[Integer]]
  deriving (Show)

instance Semigroup M22 where
  M22 a <> M22 b =
    M22 [[sum $ zipWith (*) aRow bCol | bCol <- transpose b] | aRow <- a]

instance Monoid M22 where
  mempty = M22 [[1, 0], [0, 1]]

fibMatrix :: M22
fibMatrix = M22 [[1, 1], [1, 0]]

getM22 :: M22 -> [[Integer]]
getM22 (M22 a) = a

fibEvenBetter :: Integer -> Integer
fibEvenBetter n = getM22 (power fibMatrix n) !! 0 !! 1







{-
class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m

  foldr :: (a -> b -> b) -> b -> f a -> b
  foldl :: (b -> a -> b) -> b -> f a -> b

  -- either implement foldMap or foldr
-}



data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r


example :: Tree Int
example = Node (Node Leaf 5 (Node Leaf 2 Leaf)) 3 (Node Leaf 9 Leaf)

sum'' :: Foldable f => f Int -> Int
sum'' xs = getSum $ foldMap Sum xs

product'' :: Foldable f => f Int -> Int
product'' xs = getProduct $ foldMap Product xs


-- Maybe (First a)

firstElem :: Foldable f => f a -> Maybe a
firstElem xs = fmap getFirst $ foldMap (\x -> Just (First x)) xs

firstElem' :: Foldable f => f a -> Maybe a
firstElem' xs = foldr (\x _ -> Just x) Nothing xs

-- foldr f b [1, 2, 3]
-- 1 : (2 : (3 : []))
-- f 1 (f 2 (f 3 b))



{-
foldMap :: Monoid m => (a -> m) -> f a -> m
foldMap f xs = foldr (\a m -> f a <> m) mempty xs
-}

-- foldr using foldMap:

-- endofunction
newtype Endo a = Endo (a -> a)

getEndo :: Endo a -> a -> a
getEndo (Endo f) = f

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

foldr' :: Foldable f => (a -> b -> b) -> b -> f a -> b
foldr' f b xs = getEndo (foldMap (\x -> Endo (\b -> f x b)) xs) b
  -- Endo b

-- foldr' f b [1, 2, 3] =
-- (\b -> f 1 b) . (\b -> f 2 b) . (\b -> f 3 b) . id $ b
-- (f 1) . (f 2) . (f 3) $ b
-- f 1 (f 2 (f 3 b))
