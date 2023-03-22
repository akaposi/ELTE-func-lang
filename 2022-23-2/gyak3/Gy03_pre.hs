{-# options_ghc -Wincomplete-patterns #-}
module Gy03 where

import Prelude hiding (Maybe(..), Either(..), Functor(..))


{- Typeclasses: Eq, Ord, Show -}

-- class Show a where
--   show :: a -> String

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (==) a b = not (a /= b)
--   (/=) :: a -> a -> Bool
--   (/=) a b = not (a == b)

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool
--   (>=) ...
--   ... (Use `:i Ord` for more info)


--- Color type + instances

data Color = Red | Green | Blue

instance Show Color where
  show = undefined

instance Eq Color where
  (==) = undefined

instance Ord Color where
  (<=) = undefined


-- List type + instances

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show = undefined

instance Eq a => Eq (List a) where
  (==) = undefined

instance Ord a => Ord (List a) where
  (<=) = undefined


--- Tree type + instances

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) = undefined

instance Ord a => Ord (Tree a) where
  (<=) = undefined


-- Maybe type + instances

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<=) = undefined


-- Either type + instances

data Either a b = Left a  | Right b

-- Here you need to change the instance declarations as well!
instance Show a => Show (Either a b) where
  show = undefined

instance Eq a => Eq (Either a b) where
  (==) = undefined

instance Ord a => Ord (Either a b) where
  (<=) = undefined


{- The `Functor` typeclass -}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

data    Foo1 a     = Foo1 Int a a a deriving Show
data    Foo2 a     = Foo2 Bool a Bool deriving Show
data    Foo3 a     = Foo3 a a a a a deriving Show
data    RoseTree a = RoseNode a [RoseTree a] deriving Show
data    Pair a b   = Pair a b deriving Show
data    TreeI i a  = LeafI a | NodeI (i -> TreeI i a)
newtype Id a       = Id a deriving Show
newtype Const a b  = Const a deriving Show
newtype Fun a b    = Fun (a -> b)

--

instance Functor List where
  fmap = undefined

instance Functor Maybe where
  fmap = undefined

instance Functor Tree where
  fmap = undefined

instance Functor (Either a) where
  fmap = undefined

--

instance Functor Foo1 where
  fmap = undefined

instance Functor Foo2 where
  fmap = undefined

instance Functor Foo3 where
  fmap = undefined

--

instance Functor RoseTree where
  fmap = undefined

instance Functor (Pair a) where
  fmap = undefined

instance Functor (TreeI i) where
  fmap = undefined

--

instance Functor Id where
  fmap = undefined

instance Functor (Const a) where
  fmap = undefined

instance Functor (Fun a) where
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined
