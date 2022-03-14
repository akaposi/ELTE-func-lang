{-# LANGUAGE InstanceSigs, DeriveFoldable #-}

module Lesson05 where

data List a = Nil | Cons a (List a) deriving (Foldable, Show)
infixr 5 `Cons`

data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
data RoseTree a = RoseNode a [RoseTree a]

data    Foo3 a      = Foo3 a a a a a deriving Show
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Pair a b    = Pair a b deriving Show
data    Either' a b = Left' a | Right' b deriving Show
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a deriving Show
newtype Const a b   = Const a deriving Show

-- Monad: "Mellékhatások" dinamikusak, nem tudni, hogy mi történik addig, amíg meg nem történik.

-- Törvények:
{-

-}

{-
instance Monad List where
    (>>=) = undefined

instance Monad BinaryTree where
    (>>=) = undefined

instance Monad Foo3 where
    (>>=) = undefined

instance Monad Tree1 where
    (>>=) = undefined

instance Monad Pair where
    (>>=) = undefined

instance Monad RoseTree where
    (>>=) = undefined

instance Monad Tree3 where
    (>>=) = undefined

instance Monad Either' where
    (>>=) = undefined

instance Monad Id where
    (>>=) = undefined

instance Monad Const where
    (>>=) = undefined
-}

{-

f1 :: Monad m => (a -> b) -> m a -> m b
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

-}

