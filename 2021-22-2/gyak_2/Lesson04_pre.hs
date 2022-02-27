module Lesson04 where

data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
data RoseTree a = RoseNode a [RoseTree a]
-- RoseTree Foldable

instance Foldable List where
    foldr f acc Nil         = acc
    foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Foldable BinaryTree where
    foldr f acc (Leaf a)     = f a acc -- f :: a -> b -> b ; acc :: b ; a :: a
    foldr f acc (Node l a r) = f a (foldr f (foldr f acc r) l)


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

    
-- Törvények:
-- (Haskellben nem lehet kikényszeríteni a törvények teljesülését, ahhoz más nyelv kell)

data    Foo1 a      = Foo1 Int a a a deriving Show
data    Foo2 a      = Foo2 Bool a Bool deriving Show
data    Foo3 a      = Foo3 a a a a a deriving Show
data    Tree1 a     = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show
data    Pair a b    = Pair a b
data    Either' a b = Left' a | Right' b
data    Tree3 i a   = Leaf3 a | Node3 (i -> Tree3 i a)  -- i-szeres elágazás
newtype Id a        = Id a
newtype Const a b   = Const a
newtype Fun a b     = Fun (a -> b)

{-
instance Functor List where
    fmap = undefined

instance Functor BinaryTree where
    fmap = undefined

instance Functor Foo1 where
    fmap = undefined

instance Functor Foo2 where
    fmap = undefined

instance Functor Foo3 where
    fmap = undefined

instance Functor Tree1 where
    fmap = undefined

instance Functor Pair where
    fmap = undefined

instance Functor RoseTree where
    fmap = undefined

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