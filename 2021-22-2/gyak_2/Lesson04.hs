{-# LANGUAGE InstanceSigs #-} -- ghc < 9.2.1 esetén
module Lesson04 where

data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
data RoseTree a = RoseNode a [RoseTree a]

instance Foldable List where
    foldr f acc Nil         = acc
    foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Foldable BinaryTree where
    foldr f acc (Leaf a)     = f a acc -- f :: a -> b -> b ; acc :: b ; a :: a
    foldr f acc (Node l a r) = f a (foldr f (foldr f acc r) l)

instance Foldable RoseTree where
    foldr f acc t = undefined

--------------------------------------

-- map :: (a -> b) -> [a] -> [b]
-- sima map (List map)
listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

-- tree map (BinaryTree-n)
treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f (Leaf a) = Leaf $ f a
treeMap f (Node l a r) = Node (treeMap f l) (f a) (treeMap f r)

-- Maybe map
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just a) = Just $ f a

-- pair map (melyik paraméter fölött lehet? (by nyelv deizájn))
pairMap :: (a -> b) -> (c,a) -> (c,b)
pairMap f (c,a) = (c,f a)

pairMapV2 :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairMapV2 f g (a,c) = (f a, g c) 

pairMapV3 :: (a -> b) -> (a,c) -> (b,c)
pairMapV3 f (a,c) = (f a,c) 
-- Either map (szintén)

eitherMap = undefined

-- Functor:
-- típusa, típusok típusa ((,), Maybe, Either, List ...)
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
    -- Mi a típusa f-nek?
    -- f :: Type -> Type

-- Törvények:
-- (Haskellben nem lehet kikényszeríteni a törvények teljesülését, ahhoz más nyelv kell)
{-

-}

data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool deriving Show
data    Foo3 a      = Foo3 a a a a a deriving Show
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Pair a b    = Pair a b deriving Show
data    Either' a b = Left' a | Right' b deriving Show
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a deriving Show
newtype Const a b   = Const a deriving Show
newtype Fun a b     = Fun (a -> b)

{-
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
-}

-- data SamePair a = SamePair a a

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons a as) = Cons (f a) $ fmap f as 

instance Functor BinaryTree where
    fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Functor Foo1 where
    fmap :: (a -> b) -> Foo1 a -> Foo1 b
    fmap f (Foo1 int a1 a2 a3) = Foo1 int (f a1) (f a2) (f a3)

instance Functor Foo2 where
    fmap :: (a -> b) -> Foo2 a -> Foo2 b
    fmap f (Foo2 bool a bool') = Foo2 bool (f a) bool'

instance Functor Foo3 where
    fmap :: (a -> b) -> Foo3 a -> Foo3 b
    fmap f (Foo3 a1 a2 a3 a4 a5) = Foo3 (f a1) (f a2) (f a3) (f a4) (f a5)

instance Functor Tree1 where
    fmap :: (a -> b) -> Tree1 a -> Tree1 b
    fmap f (Leaf1 a) = Leaf1 $ f a
    fmap f (Node1 t1 t2) = Node1 (fmap f t1) (fmap f t2)

instance Functor (Pair c) where
    fmap :: (a -> b) -> Pair c a -> Pair c b
    fmap f (Pair c a) = Pair c $ f a

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (RoseNode a as) = RoseNode (f a) (map (fmap f) as)
{-
instance Functor Tree3 where
    fmap = undefined

instance Functor Either' where
    fmap = undefined

instance Functor Id where
    fmap = undefined

instance Functor Const where
    fmap = undefined

instance Functor Fun where
    fmap = undefined
-}

-- Applicative: Minden "mellékhatás" statikusan ismert

-- Törvények:
{-

-}

{-
instance Applicative List where
    pure = undefined
    (<*>) = undefined

instance Applicative BinaryTree where
    pure = undefined
    (<*>) = undefined

instance Applicative Foo1 where
    pure = undefined
    (<*>) = undefined

instance Applicative Foo2 where
    pure = undefined
    (<*>) = undefined

instance Applicative Foo3 where
    pure = undefined
    (<*>) = undefined

instance Applicative Tree1 where
    pure = undefined
    (<*>) = undefined

instance Applicative Pair where
    pure = undefined
    (<*>) = undefined

instance Applicative RoseTree where
    pure = undefined
    (<*>) = undefined

instance Applicative Tree3 where
    pure = undefined
    (<*>) = undefined

instance Applicative Either' where
    pure = undefined
    (<*>) = undefined

instance Applicative Id where
    pure = undefined
    (<*>) = undefined

instance Applicative Const where
    pure = undefined
    (<*>) = undefined

instance Applicative Fun where
    pure = undefined
    (<*>) = undefined
-}