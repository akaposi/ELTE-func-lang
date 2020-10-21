{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes07 where

import Control.Monad (ap, forM, forM_, liftM2)
import Control.Monad.State

-------------------------------------------------------------------------------

-- class Semigroup m where
--   (<>) :: m -> m -> m

-- class Monoid m where
--   mempty :: m

-- instance Semigroup [] where (<>) = (++)
-- instance Monoid [] where mempty = []

-- class Foldable f where
--   foldMap :: Monoid m => (m -> a) -> f a -> m
--   foldr   :: (a -> b -> b) -> b -> f a -> b

-- Define foldMap using foldr
foldMapFromFoldr :: (Foldable f, Monoid m) => (m -> a) -> f a -> m
foldMapFromFoldr = undefined

-- Define the functions null', length' and toList' using foldr
null' :: Foldable f => f a -> Bool
null' = undefined

length' :: Foldable f => f a -> Int
length' = undefined

toList' :: Foldable f => f a -> [a]
toList' = undefined

-- Define the functions null'', length'' and toList'' using foldMap
null'' :: Foldable f => f a -> Bool
null'' = undefined

length'' :: Foldable f => f a -> Int
length'' = undefined

toList'' :: Foldable f => f a -> [a]
toList'' = undefined

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