{-# options_ghc -Wincomplete-patterns #-}
module Gy03 where

import Prelude hiding (Either(..), Functor(..))


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


-- List type + instances

data List a = Nil | Cons a (List a)

exampleList :: List Int
exampleList = Cons 5 (Cons 2 (Cons 7 Nil))
instance Show a => Show (List a) where
  show Nil = "Nil"
  show (Cons a ls) = "Cons " ++ show a ++ " (" ++ show ls ++ ")"

instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons a ls) (Cons a' ls') = a == a' && ls == ls'
  (==) _ _ = False

instance Ord a => Ord (List a) where
  (<=) Nil Nil = True
  (<=) Nil (Cons a ls) = True
  (<=) (Cons a ls) Nil = False
  (<=) (Cons a ls) (Cons a' ls') = a < a' || (a == a' && ls <= ls')


--- Tree type + instances

data Tree a = Leaf a | Node (Tree a) (Tree a)
exampleTree :: Tree Int
exampleTree = Node (Node (Leaf 4) (Node (Leaf 2) (Leaf 6))) (Node (Leaf 3) (Leaf 5))

instance Show a => Show (Tree a) where
  show (Leaf a) = "Leaf " ++ show a
  show (Node tr1 tr2) = "(" ++ show tr1 ++ "-" ++ show tr2 ++ ")"

instance Eq a => Eq (Tree a) where
  (==) (Leaf a) (Leaf a') = a == a'
  (==) (Node tr1 tr2) (Node tr1' tr2') = tr1 == tr1' && tr2 == tr2'
  (==) _ _ = False

instance Ord a => Ord (Tree a) where
  (<=) (Leaf a) (Leaf a') = a <= a'
  (<=) (Leaf a) (Node tr1 tr2) = True
  (<=) (Node tr1 tr2) (Leaf a) = False
  (<=) (Node tr1 tr2) (Node tr1' tr2') = tr1 < tr1' || (tr1 == tr1' && tr2 <= tr2')
--  (<=) (Node tr1 tr2) (Node tr1' tr2') = (tr1 <= tr2) <= (tr1' <= tr2')

-- Either type + instances

data Either a b = Left a  | Right b 
    deriving (Show, Eq, Ord)

-- Here you need to change the instance declarations as well!
-- instance (Show a, Show b) => Show (Either a b) where
--   show = undefined

-- instance (Eq a, Eq b) => Eq (Either a b) where
--   (==) = undefined

-- instance (Ord a, Ord b) => Ord (Either a b) where
--   (<=) = undefined


{- The `Functor` typeclass -}

-- Functor laws:
-- fmap id x = x
-- fmap (f . g) x = (fmap f . fmap g) x 
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
  fmap f Nil = Nil
  fmap f (Cons a ls) = Cons (f a) (fmap f ls)


instance Functor Maybe where
--fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor Tree where
  fmap = undefined

instance Functor (Either a) where
  fmap f (Left a) = Left a
  fmap f (Right b) = Right (f b)

--

instance Functor Foo1 where
  fmap f (Foo1 n a1 a2 a3) = Foo1 n (f a1) (f a2) (f a3)

instance Functor Foo2 where
  fmap f (Foo2 b a b') = Foo2 b (f a) b'

instance Functor Foo3 where
  fmap = undefined


-- data    RoseTree a = RoseNode a [RoseTree a] deriving Show

instance Functor RoseTree where
 fmap f (RoseNode a rs) = RoseNode (f a) (map (fmap f) rs) 

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


-- Foldable típusosztály ----------------------------------------------------------------

-- class Foldable f where
--   foldr :: (a -> b -> b) -> b -> f a -> b
--   foldl
--   foldMap
--   ...

instance Foldable List where
--foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f z l = undefined

instance Foldable Tree where
--foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z t = undefined