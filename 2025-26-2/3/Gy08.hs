{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving #-}
{- HLINT ignore "Use newtype instead of data" -}
module Gy08 where

import Prelude hiding (Maybe(..), Either(..))

-- Previous type classes:
-- Functor (fmap :: (a -> b) -> f a -> f b)
-- Foldable
-- Monad ((>>==) :: m a -> (a -> m b) -> m b, return :: a -> m a)
-- Today:
-- Applicative, Traversable

-- Define a function that maps an "effectful" function over a list
--                                          v collect the m side effects
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList _ [] = return []
mapMList f (x : xs) = do 
  fx <- f x
  fxs <- mapMList f xs
  return (fx : fxs)

-- Since Functor (plain mapping) was generalizable, this effectful mapping can also be generalized.
data Single a = Single a deriving (Eq, Show, Functor, Foldable)
data Tuple a = Tuple a a deriving (Eq, Show, Functor, Foldable)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show, Functor, Foldable)
data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor, Foldable)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor, Foldable)

-- Define these functions for the types above
mapMSingle :: Monad m => (a -> m b) -> Single a -> m (Single b)
mapMSingle f (Single a) = f a >>= (\b -> return $ Single b)
{-
do
  b <- f a
  return $ Single b
-}

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f (Tuple a a') = do 
  fa <- f a 
  fa' <- f a' 
  return $ Tuple fa fa'

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple f (Quintuple x y z u v) = do 
  fx <- f x 
  fy <- f y 
  fz <- f z
  fu <- f u 
  fv <- f v 
  return $ Quintuple fx fy fz fu fv

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe _ Nothing = return Nothing
mapMMaybe f (Just a) = do
  b <- f a 
  return $ Just b

-- However, Monad binding allows more than we need to define these operations
-- The main operation of the monad is (>>=) :: m a -> (a -> m b) -> m b
-- Which models your dependencies between side effects
-- However, in mapping, there is no result-based dependency between individual elements
-- Applicative type class: Between Functor and Monad
{-
:i Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘Control.Applicative’
-}
-- The concept of Applicative type class is a generalization of the fmap operation to functions with arbitrary parameters, e.g.:
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- However, this requires a combination of (independent) side effects
-- liftA operations are only available up to liftA3 in the standard library, but arbitrary liftA can be written using <*>
-- e.g.:
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 func fa fb fc fd = func <$> fa <*> fb <*> fc <*> fd
-- func :: a -> (b -> c -> d -> e)
-- func <$> fa :: f (b -> c -> d -> e)
-- func <$> fa <*> fb :: f (c -> d -> e)
-- func <$> fa <*> fb <*> fc :: f (d -> e)
-- func <$> fa <*> fb <*> fc <*> fd :: f e
-- We will use these so-called app chains when writing mapA as well!
-- Let's write the mapM operation using only an Applicative constraint
-- The algorithm is the same as for the functor, only instead of function application, we use <*> and pure for independent values

-- fmap :: (a -> b) -> List a -> List b
-- fmap _ Nil = Nil
-- fmap f (Cons x xs) = Cons (f x) (fmap f xs)

mapA :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA _ Nil = pure Nil 
mapA func (Cons a as) = Cons <$> (func a) <*> (mapA func as)
-- Cons :: forall a. a -> List a -> List a
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- func a :: f b
-- Cons <$> (func a) :: f (List b -> List b)

-- This effectful mappability property will be the so-called Traversable type class.
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in ‘Data.Traversable’
-}

-- Tasks

instance Traversable Single where
  traverse :: Applicative f => (a -> f b) -> Single a -> f (Single b)
  traverse g (Single a) = Single <$> g a

  sequenceA :: Applicative f => Single (f a) -> f (Single a)
  sequenceA (Single fa) = Single <$> fa

instance Traversable Tuple where
  traverse :: Applicative f => (a -> f b) -> Tuple a -> f (Tuple b)
  traverse f (Tuple a a') = (Tuple <$> (f a)) <*> (f a')

instance Traversable Quintuple where
  traverse :: Applicative f => (a -> f b) -> Quintuple a -> f (Quintuple b)
  traverse f (Quintuple x y z u v) = Quintuple <$> f x <*> f y <*> f z <*> f u <*> f v

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = Just <$> f a

data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor, Foldable)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show, Functor, Foldable)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Functor, Foldable)
data Either e a = Left e | Right a deriving (Eq, Show, Functor, Foldable)
data BiTuple e a = BiTuple e a deriving (Eq, Show, Functor, Foldable)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show, Functor, Foldable)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show, Functor, Foldable)
data Apply f a = MkApply (f a) deriving (Eq, Show, Functor, Foldable)
data Fix f a = MkFix (f (Fix f a)) deriving (Functor, Foldable)
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show, Functor, Foldable)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show, Functor, Foldable)
data FList f a = FNil | FCons (f a) (f (FList f a)) deriving (Functor, Foldable)

-- Tasks

instance Traversable NonEmpty where

instance Traversable NonEmpty2 where

instance Traversable Tree where

instance Traversable (Either fixed) where
  traverse :: Applicative f => (a -> f b) -> Either fixed a -> f (Either fixed b)
  traverse _ (Left fix) = pure $ Left fix
  traverse f (Right a) = Right <$> f a

instance Traversable (BiTuple fixed) where

instance Traversable (TriEither fixed1 fixed2) where

instance Traversable (BiList fixed) where

-- Higher order constraints
instance Traversable f => Traversable (Apply f) where
  traverse :: (Traversable f, Applicative g) => (a -> g b) -> Apply f a -> g (Apply f b)
  traverse func (MkApply fa) = MkApply <$> traverse func fa
  --                                       ^ f's traverse
  -- Traversable f means traverse :: Applicative g => (a -> g b) -> f a -> g (f b)

instance Traversable f => Traversable (Fix f) where

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: (Traversable f, Traversable g, Applicative h) =>
              (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse func (MkCompose fga) = MkCompose <$> traverse (traverse func) fga
  --                                            ^ f's traverse
  --                                                      ^ g's traverse
  -- Traversable f means traverse :: Applicative p => (a -> p b) -> f a -> p (f b)
  -- Set p := h, a := g a, b := g b, then (g a -> h b) -> f (g a) -> h (f (g b))
  -- Traversable g means traverse :: Applicative q => (a -> q b) -> g a -> q (g b)
  -- Set q := h

instance Traversable f => Traversable (Sum f fixed) where

instance Traversable f => Traversable (Prod f fixed) where

instance Traversable f => Traversable (FList f) where


-- Extra: Applicative Do

-- Magic, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
