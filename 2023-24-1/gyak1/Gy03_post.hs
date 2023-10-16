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
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Eq Color where
  (/=) Red Red = False
  (/=) Green Green = False
  (/=) Blue Blue = False
  (/=) _ _ = True

instance Ord Color where
  (<=) Red _ = True
  (<=) _ Blue = True
  (<=) Green Green = True
  (<=) _ _ = False


-- List type + instances

l1 :: List Int
l1 = Cons 1 (Cons 2 (Cons 3 Nil))

l2 :: List Int
l2 = Cons 2 (Cons 3 Nil)

l3 :: List Int
l3 = Nil

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show Nil = "[]"
  show (Cons a as) = show a ++ " -- " ++ show as

instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons a as) (Cons a' as') = a == a' && as == as'
  (==) _ _ = False

instance Ord a => Ord (List a) where
  (<=) Nil Nil = True
  (<=) Nil (Cons a as) = True
  (<=) (Cons a as) Nil = False
  (<=) (Cons a as) (Cons a' as') = a < a' || (a == a' && as <= as')


--- Tree type + instances

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

-- instance Show a => Show (Tree a) where
--   show = undefined

instance Eq a => Eq (Tree a) where
  (==) (Leaf a) (Leaf a') = a == a'
  (==) (Node l r) (Node l' r') = l == l' && r == r'
  (==) _ _ = False

instance Ord a => Ord (Tree a) where
  (<=) (Leaf a) (Leaf a') = a <= a'
  (<=) (Leaf a) (Node l r) = True
  (<=) (Node l r) (Leaf a) = False
  (<=) (Node l r) (Node l' r') = l < l' || (l == l' && r <= r')


-- Maybe type + instances

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
  (==) Nothing Nothing = True
  (==) (Just a) (Just a') = a == a'
  (==) _ _ = False

instance Ord a => Ord (Maybe a) where
  (<=) Nothing _ = True
  (<=) (Just a) Nothing = False
  (<=) (Just a) (Just a') = a <= a'


-- Either type + instances

data Either a b = Left a  | Right b

-- Here you need to change the instance declarations as well!
instance (Show a, Show b) => Show (Either a b) where
  show (Left a) = "Left " ++ show a
  show (Right b) = "Right " ++ show b

instance (Eq a, Eq b) => Eq (Either a b) where
  (==) (Left a) (Left a') = a == a'
  (==) (Right b) (Right b') = b == b'
  (==) _ _ = False

instance (Ord a, Ord b) => Ord (Either a b) where
  (<=) (Left a) (Left a') = a <= a'
  (<=) (Left a) (Right b) = True 
  (<=) (Right b) (Left a) = False
  (<=) (Right b) (Right b') = b <= b' 


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

-- 2 functor törvény van

-- 1. fmap id x = x
-- 2. fmap f (fmap g x) = fmap (f . g) x

-- data List a = Nil | Cons a (List a)
instance Functor List where
--  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor Tree where
  -- fmap :: (a -> b) -> f a -> f b -- általános
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Functor (Either a) where
  -- fmap :: (b -> c) -> f b -> f c
  -- fmap :: (b -> c) -> Either a b -> Either a c
  fmap f (Left a) = Left a
  fmap f (Right b) = Right (f b)

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