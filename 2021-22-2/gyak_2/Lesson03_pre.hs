module Lesson03 where

data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista

listSum :: Num a => List a -> a
listSum = undefined

{-
t =
              adat1
            /      \
        adat2      adat3
        /  \
    adat4  adat5
-}
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)
-- ez mindenképpen két irányba ágazik, olyan nincs, hogy csak egy gyereke van. Vagy 0 vagy 2 gyerek.

t :: BinaryTree String
t = Node (Node (Leaf "adat4") "adat2" (Leaf "adat5")) "adat1" (Leaf "adat3")

t2 :: Num a => BinaryTree a
t2 = Node (Node (Leaf 2) 3 (Leaf 5)) 7 (Leaf 11)

height :: Num b => BinaryTree a -> b
height = undefined

treeSum :: Num a => BinaryTree a -> a
treeSum = undefined

data RoseTree

-- írd meg a következő instance-okat
instance Eq a => Eq (BinaryTree a) where
    (==) = undefined

{-
    1
   / \
  2   3
sorrendben (preorder bejárás)
-}
instance Ord a => Ord (BinaryTree a) where
    (<=) = undefined

instance Show a => Show (BinaryTree a) where
    show = undefined

{-
instance Eq RoseTree where
    (==) = undefined

instance Ord RoseTree where
    (<=) = undefined

instance Show RoseTree where
    show = undefined
-}

--------------------------------------

instance Foldable List where
    foldr = undefined

instance Foldable BinaryTree where
    foldr = undefined

listSum' :: Num a => List a -> a
listSum' = undefined

treeSum' :: Num a => BinaryTree a -> a
treeSum' = undefined

-- RoseTree Foldable

--------------------------------------

-- sima map (List map)
listMap = undefined

-- tree map (BinaryTree-n)
treeMap = undefined

-- Maybe map
maybeMap = undefined

-- pair map (melyik paraméter fölött lehet? (by nyelv deizájn))
pairMap = undefined

-- Either map (szintén)
eitherMap = undefined

-- Functor:
-- típusa, típusok típusa ((,), Maybe, Either, List ...)

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