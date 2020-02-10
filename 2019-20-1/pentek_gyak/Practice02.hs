{-# LANGUAGE FlexibleInstances, KindSignatures, InstanceSigs #-}
module Practice02 where

import Prelude hiding (Semigroup(..), Monoid(..), Functor(..))

class Semigroup (m :: *) where
  -- mappend 
  (<>) :: m -> m -> m 

  -- associativity law:
  -- (a <> b) <> c == a <> (b <> c)

newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Show)

newtype Prod a = Prod { getProd :: a }
  deriving (Eq, Ord, Show)

instance Semigroup (Sum Int) where 
  (<>) (Sum lhs) (Sum rhs) = Sum (lhs + rhs)

instance Semigroup (Prod Int) where 
  (<>) (Prod lhs) (Prod rhs) = Prod (lhs * rhs)

-- HW: Semigroup for [a]

class Semigroup m => Monoid m where 
  mempty :: m

  -- identity element
  -- mempty <> x == x 
  -- x <> mempty == x 

instance Monoid (Sum Int) where 
  mempty = Sum 0

instance Monoid (Prod Int) where 
  mempty = Prod 1

-- HW: Monoid for [a]

class Functor (f :: * -> *) where 
  fmap :: (a -> b) -> f a -> f b

data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Semigroup (List a) where 
  (<>) :: List a -> List a -> List a
  (<>) = error "See HW: (+++)"

instance Monoid (List a) where 
  mempty :: List a
  mempty = Nil

instance Functor List where
  fmap :: (a -> b) -> List a -> List b 
  fmap f Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 
