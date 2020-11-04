{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes07 where

import Control.Monad (ap, forM, forM_, liftM2)
import Control.Monad.State

-------------------------------------------------------------------------------

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
  deriving (Functor, Foldable, Traversable)

-- class Semigroup m where
--   (<>) :: m -> m -> m

-- class Semigroup m => Monoid m where
--   mempty :: m

-- instance Semigroup [a] where (<>) = (++)
-- instance Monoid [a] where mempty = []

-- class Foldable f where
--   foldMap :: Monoid m => (a -> m) -> f a -> m
--   foldr   :: (a -> b -> b) -> b -> f a -> b

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f []     = mempty
foldMapList f (x:xs) = f x <> foldMapList f xs

-- foldMapList f [x, y, z] == f x <> f y <> f z <> mempty == f x <> f y <> f z

-- Define foldMap using foldr
foldMapFromFoldr :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMapFromFoldr f xs = foldr (\a m -> f a <> m) mempty xs

-- Define the functions null', length' and toList' using foldr
null' :: Foldable f => f a -> Bool
null' xs = foldr (\_ _ -> False) True xs

length' :: Foldable f => f a -> Int
length' xs = foldr (\_ x -> x+1) 0 xs

toList' :: Foldable f => f a -> [a]
toList' xs = foldr (\y ys -> y : ys) [] xs

-- Define the functions null'', length'' and toList'' using foldMap
-- Hint : you should define some monoid instances for Bool and Int

newtype And = And { getAnd :: Bool }
            deriving (Show, Eq, Ord)
instance Semigroup And where x <> y = And $ getAnd x && getAnd y
instance Monoid And    where mempty = And True

null'' :: Foldable f => f a -> Bool
null'' xs = getAnd $ foldMap (\_ -> And False) xs

-- could be called Or
newtype Any = Any { getAny :: Bool }
            deriving (Show, Eq, Ord)
instance Semigroup Any where x <> y = Any $ getAny x || getAny y
instance Monoid Any    where mempty = Any False

notNull'' :: Foldable f => f a -> Bool
notNull'' xs = getAny $ foldMap (\_ -> Any True) xs

newtype Sum a = Sum { getSum :: a }
              deriving (Show, Eq, Ord)
instance Num a => Semigroup (Sum a) where x <> y = Sum $ getSum x + getSum y
instance Num a => Monoid (Sum a)    where mempty = Sum 0

length'' :: Foldable f => f a -> Int
length'' xs = getSum $ foldMap (\_ -> Sum 1) xs

toList'' :: Foldable f => f a -> [a]
toList'' = foldMap (\x -> [x])

-- Define the following functions on Foldable
--   Try to use both foldr and foldMap

sum' :: (Num a, Foldable f) => f a -> a
sum' = getSum . foldMap Sum

-- firstElem p xs should return the first element of xs that satisfies the predicate p. 
--   Just x means that this element was x.
--   Nothing means that there was no such element.
newtype First a = First { getFirst :: Maybe a }
instance Semigroup (First a) where
  First Nothing <> b = b
  a             <> _ = a
instance Monoid (First a) where
  mempty = First Nothing

firstElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
firstElem p = getFirst . foldMap (\x -> if p x then First (Just x) else First Nothing)

-- lastElem p xs should return the last element of xs that satisfies the predicate p. 
newtype Last a = Last { getLast :: Maybe a }
instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b            = b
instance Monoid (Last a) where
  mempty = Last Nothing

lastElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
lastElem p = getLast . foldMap (\x -> if p x then Last (Just x) else Last Nothing)

newtype Product a = Product { getProduct :: a }
                  deriving (Show, Eq, Ord)
instance Num a => Semigroup (Product a) where x <> y = Product $ getProduct x * getProduct y
instance Num a => Monoid (Product a) where mempty = Product 1

product' :: (Num a, Foldable f) => f a -> a
product' = getProduct . foldMap Product

-- maximum' xs should return the maximum element of xs.
--   Nothing means that xs was empty.
newtype Max a = Max { getMax :: Maybe a }
instance Ord a => Semigroup (Max a) where
  Max (Just a) <> Max (Just b) = Max (Just (max a b))
  Max (Just a) <> Max Nothing  = Max (Just a)
  Max Nothing  <> b            = b
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

maximum' :: (Ord a, Foldable f) => f a -> Maybe a
maximum' = getMax . foldMap (Max . Just)

-- class Traversable f where
--   traverse :: Applicative m => (a -> m b) -> f a -> m (f b)

-- Bonus: define foldr using foldMap
data Endo a = Endo { getEndo :: a -> a }
instance Semigroup (Endo a) where Endo f <> Endo g = Endo (f . g)
instance Monoid (Endo a) where mempty = Endo id

foldrFromFoldMap :: (Foldable f) => (a -> b -> b) -> b -> f a -> b
foldrFromFoldMap f x t = getEndo (foldMap (\x -> Endo (f x)) t) x

-- Bonus: define foldMap using traverse
newtype Writer m a = Writer { getWriter :: (m,a) }
                  deriving (Functor)
instance Monoid m => Applicative (Writer m) where
  pure x = Writer (mempty, x)
  Writer (m1, f) <*> Writer (m2, x) = Writer (m1 <> m2, f x)

foldMapFromTraverse :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMapFromTraverse f t = fst (getWriter (traverse (\x -> Writer (f x, ())) t))

-- Bonus: define fmap using traverse
newtype Id a = Id { getId :: a }
            deriving (Functor)
instance Applicative Id where
  pure x = Id x
  Id f <*> Id x = Id (f x)

fmapFromTraverse :: (Traversable f) => (a -> b) -> f a -> f b
fmapFromTraverse f t = getId (traverse (Id . f) t)

--------------------------------------------------------------------------------