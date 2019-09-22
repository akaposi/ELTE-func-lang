{-#LANGUAGE FlexibleInstances, FlexibleContexts, KindSignatures, InstanceSigs #-}
module Practice02 where

import Prelude hiding (Monoid(..), Semigroup(..), Functor(..))

fact :: Int -> Int
fact 0 = 1
fact n = fact (n - 1) * n

{-
data Product a b = Product a b
data Union a b = Left a | Right b
data Void
type Bool = Union Unit Unit
            Left Unit
            Right Unit

data Exp a b = Fun (a -> b)
-}

data List a = Nil
            | Cons a (List a)
  deriving (Show, Eq, Ord)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

class Semigroup m where
  -- mappend
  (<>) :: m -> m -> m

newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq, Ord)

newtype Prod a = Prod { getProd :: a }
  deriving (Show, Eq, Ord)

instance Semigroup (Sum Int) where
  (<>) (Sum n) (Sum k) = Sum (n + k)

instance Semigroup (Prod Int) where
  (<>) (Prod n) (Prod k) = Prod (n * k)

class Semigroup m => Monoid m where
  mempty :: m

instance Monoid (Sum Int) where
  mempty = Sum 0

instance Monoid (Prod Int) where
  mempty = Prod 1

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
