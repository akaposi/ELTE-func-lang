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
foldMapFromFoldr = undefined

-- Define the functions null', length' and toList' using foldr
null' :: Foldable f => f a -> Bool
null' = undefined

length' :: Foldable f => f a -> Int
length' = undefined

toList' :: Foldable f => f a -> [a]
toList' = undefined

-- Define the functions null'', length'' and toList'' using foldMap
-- Hint : you should define some monoid instances for Bool and Int
null'' :: Foldable f => f a -> Bool
null'' = undefined

length'' :: Foldable f => f a -> Int
length'' = undefined

toList'' :: Foldable f => f a -> [a]
toList'' = foldMap (\x -> [x])

-- Define the following functions on Foldable
--   Try to use both foldr and foldMap

-- firstElem p xs should return the first element of xs that satisfies the predicate p. 
--   Just x means that this element was x.
--   Nothing means that there was no such element.
firstElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
firstElem = undefined

-- lastElem p xs should return the last element of xs that satisfies the predicate p. 
lastElem :: Foldable f => (a -> Bool) -> f a -> Maybe a
lastElem = undefined

sum' :: (Num a, Foldable f) => f a -> a
sum' = undefined

product' :: (Num a, Foldable f) => f a -> a
product' = undefined

-- maximum' xs should return the maximum element of xs.
--   Nothing means that xs was empty.
maximum' :: (Ord a, Foldable f) => f a -> Maybe a
maximum' = undefined

-- class Traversable f where
--   traverse :: Applicative m => (a -> m b) -> f a -> m (f b)

-- Bonus: define foldr using foldMap
foldrFromFoldMap :: (Foldable f) => (a -> b -> b) -> b -> f a -> b
foldrFromFoldMap = undefined

-- Bonus: define foldMap using traverse
foldMapFromTraverse :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMapFromTraverse = undefined

-- Bonus: define fmap using traverse
fmapFromTraverse :: (Traversable f) => (a -> b) -> f a -> f b
fmapFromTraverse = undefined

--------------------------------------------------------------------------------