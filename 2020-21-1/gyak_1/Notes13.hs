{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Strict
import Data.Foldable

-- Extra feladatok

-- 1. Definiáld a foldr-t foldMap felhasználásával!
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr' = undefined

-- 2. Definiáld a foldMap-et foldr felhasználásával!
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined

-- 3. definiáld az fmap-et traverse felhasználásával!
fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' = undefined


-- 4. definiáld a foldr-t traverse felhasználásával!
foldr'' :: Traversable t => (a -> b -> b) -> b -> t a -> b
foldr'' = undefined
