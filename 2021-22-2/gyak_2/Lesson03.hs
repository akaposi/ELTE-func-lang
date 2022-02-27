module Lesson03 where

import Data.Typeable

data List a = Nil | Cons a (List a) -- rendes megszokott láncolt lista

listSum :: Num a => List a -> a
listSum Nil         = 0
listSum (Cons x xs) = x + listSum xs

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

height :: (Ord b, Num b) => BinaryTree a -> b
height (Leaf _) = 0
height (Node l _ r) = 1 + max (height l) (height r)

treeSum :: Num a => BinaryTree a -> a
treeSum (Leaf a) = a
treeSum (Node l a r) = treeSum l + a + treeSum r

-- írd meg a következő instance-okat
instance Eq a => Eq (BinaryTree a) where
    Leaf a       == Leaf b       = a == b
    Node l1 a r1 == Node l2 b r2 = l1 == l2 && a == b && r1 == r2
    _            == _            = False                  -- ^ Eq a => BinaryTree a -> BinaryTree a -> Bool

{-
    1
   / \
  2   3
sorrendben (preorder bejárás)
-}

getData :: BinaryTree a -> a
getData (Leaf a) = a
getData (Node l a r) = a

instance Ord a => Ord (BinaryTree a) where 
    Leaf a       <= Leaf b       = a <= b
    Leaf a       <= Node l b r   = a <= b
    Node l1 a r1 <= Leaf b       = a <= b
    Node l1 a r1 <= Node l2 b r2 = a <= b && l1 <= l2 && r1 <= r2
        -- [1,2,3] <= [2,1,3] == True, mert 1 <= 2
        -- [1,2] <= [1,2,3] == True, mert első lista struktúrája kisebb
        -- [2] <= [1,2] == False, mert 2 > 1

{-
t = Node (Node (Leaf "adat4") "adat2" (Leaf "adat5")) "adat1" (Leaf "adat3")

adat1
- adat2
-- adat4
-- adat5
- adat3

-}
instance Show a => Show (BinaryTree a) where
    show xs = showTree xs 0 where
        showTree (Leaf a) n     = replicate n '-' ++ (if n > 0 then " " else "") ++ show a
        showTree (Node l a r) n = replicate n '-' ++ (if n > 0 then " " else "") ++ show a ++ "\n" ++ showTree l (n + 1) ++ "\n" ++ showTree r (n + 1)

data RoseTree a = RoseNode a [RoseTree a]

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
    foldr f acc Nil         = acc
    foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Foldable BinaryTree where
    foldr f acc (Leaf a)     = f a acc -- f :: a -> b -> b ; acc :: b ; a :: a
    foldr f acc (Node l a r) = f a (foldr f (foldr f acc r) l)
{-
listSum :: Num a => List a -> a
listSum Nil         = 0
listSum (Cons x xs) = x + listSum xs

listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

listMap' :: (a -> b) -> List a -> List b
listMap' f = foldr (\x acc -> Cons (f x) acc) Nil
-}

listProd :: Num a => List a -> a
listProd Nil         = 1
listProd (Cons x xs) = x * listProd xs

listSum' :: Num a => List a -> a
listSum' = foldr (+) 0

listSum'' :: Num a => List a -> a
listSum'' = sum

treeSum' :: Num a => BinaryTree a -> a
treeSum' = sum

-- RoseTree Foldable

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

----------------------------------------
-- String-ek köré nem írja ki az idézőjelet kiíratáskor:

showTree' :: (Show a, Typeable a) => BinaryTree a -> String
showTree' xs = showTree'' xs 0 where
    showTree'' (Leaf a) n = replicate n '-' ++ (if n > 0 then " " else "") ++ case (cast a :: Maybe String) of
        Just s -> s
        Nothing -> show a
    showTree'' (Node l a r) n = (\b -> replicate n '-' ++ (if n > 0 then " " else "") ++ b ++ "\n" ++ showTree'' l (n + 1) ++ "\n" ++ showTree'' r (n + 1)) (case (cast a :: Maybe String) of
        Just s -> s
        Nothing -> show a)