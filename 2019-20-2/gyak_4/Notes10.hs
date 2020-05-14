{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveTraversable, DeriveFoldable, DeriveFunctor #-}
module Notes10 where

import Control.Monad
import Data.Foldable
import Data.Monoid

-- class Foldable (t :: * -> *) where
--   fold :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   foldr :: (a -> b -> b) -> b -> t a -> b
--   foldr' :: (a -> b -> b) -> b -> t a -> b
--   foldl :: (b -> a -> b) -> b -> t a -> b
--   foldl' :: (b -> a -> b) -> b -> t a -> b
--   foldr1 :: (a -> a -> a) -> t a -> a
--   foldl1 :: (a -> a -> a) -> t a -> a

-- class (Functor t, Foldable t) => Traversable (t :: * -> *) where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
--   sequence :: Monad m => t (m a) -> m (t a)

data F b = A b b
         | B
         deriving (Functor) -- , Foldable, Traversable)

instance Foldable F where
  fold :: Monoid m => F m -> m
  fold (A x y) = x <> y
  fold B       = mempty
 
  foldMap f = fold . fmap f

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<$>) = fmap
instance Traversable F where
  traverse :: Applicative f => (a -> f b) -> F a -> f (F b)
  -- traverse f (A x y) = A <$> f x <*> f y
  -- Add {-# LANGUAGE ApplicativeDo #-} at the top of the file.
  traverse f (A x y) = do
    x' <- f x
    y' <- f y
    pure (A x' y')
  traverse _ B       = pure B



toListF :: Foldable t => t a -> [a]
-- toListF = foldMap (\x -> [x])
toListF = foldr (:) []

lengthF :: Foldable t => t a -> Int
-- lengthF = length . toListF
-- lengthF = foldr (\_ y -> 1 + y) 0
lengthF t = getSum $ foldMap (const (Sum 1)) t

findFirst :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findFirst f t = getFirst $ foldMap (\a -> if f a
--                                           then First (Just a)
--                                           else mempty) t
findFirst f t = foldr (\a b -> if f a then Just a else b) Nothing t


findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast f t = getLast $ foldMap (\a -> if f a
--                                         then Last (Just a)
--                                         else mempty) t
findLast f t = foldl (\b a -> if f a then Just a else b) Nothing t

data Tree1 a  = Leaf1 a
              | Node1 (Tree1 a) (Tree1 a)
              deriving (Show)

instance Functor Tree1 where
  fmap f (Leaf1 x) = Leaf1 (f x)
  fmap f (Node1 l r) = Node1 (fmap f l) (fmap f r)
instance Foldable Tree1 where
  fold = undefined
  foldMap f = fold . fmap f
instance Traversable Tree1 where
  traverse = undefined


data Tree2 a  = Node2 a [Tree2 a]
              deriving (Show)

instance Functor Tree2 where
  fmap f (Node2 a xs) = Node2 (f a) (fmap (fmap f) xs)
instance Foldable Tree2 where
  fold :: Monoid m => Tree2 m -> m

  fold (Node2 x ts) = x <> foldMap fold ts
 
  -- fold (Node2 x []) = x
  -- fold (Node2 x (t:ts)) = fold t <> fold (Node2 x ts)

  foldMap f = fold . fmap f
instance Traversable Tree2 where
  traverse = undefined

