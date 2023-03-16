{-# language InstanceSigs #-}
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
-- 🔴🟢🔵
data Color = Red | Green | Blue

instance Show Color where
  -- show = undefined 
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False

instance Ord Color where
  (<=) Red _ = True
  (<=) _ Blue = True
  (<=) Green Green = True
  (<=) _ _ = False


-- List type + instances

--            []     (:)
data List a = Nil | Cons a (List a)
l1 :: List Int
l1 = Cons 3 (Cons 2 (Cons 1 Nil))

instance Show a => Show (List a) where
  show Nil = "{}"
  show (Cons a ls) = "(" ++ show a ++ ")-" ++ show ls

instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons a ls) (Cons a' ls') = a == a' && ls == ls'
  (==) _ _ = False

-- lexikografikus
-- 312313 <= 531 -> igaz -> 3 <= 5
instance Ord a => Ord (List a) where
  (<=) Nil _ = True
  -- (<=) _ Nil = False 
  -- (<=) (Cons a ls) (Cons a' ls') = a <= a' && ls <= ls'  Van olyan, hogy a < b és b < a
  (<=) (Cons a ls) (Cons a' ls') = (a < a') || (a == a' && ls <= ls')
  (<=) _ _ = False


--- Tree type + instances

data Tree a = Leaf a | Node (Tree a) (Tree a)
t1 :: Tree Int
t1 = Node (Leaf 5) (Node (Leaf 6) (Leaf 3))

instance Show a => Show (Tree a) where
  show (Leaf a) = "(" ++ show a ++ ")"
  show (Node l r) = "[" ++ show l ++ "---" ++ show r ++ "]"

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
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<=) = undefined


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

-- fmap id x == x
-- fmap f (fmap g x) == fmap (f . g) x 

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
  -- List a = Nil | Cons a (List a)
  fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just a) = Just $ f a

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- Either a b = Left a | Left b
instance Functor (Either a) where -- Létezik a BiFunctor
  fmap :: (b -> c) -> (Either a) b -> (Either a) c
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
