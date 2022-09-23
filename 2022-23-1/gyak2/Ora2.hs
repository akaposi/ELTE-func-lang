{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Ora2 where

import Prelude hiding (NonEmpty(..))
 
-- Definiáljunk egy NonEmpty rekurzív/induktív lista típust! 
-- Két konstruktora legyen:
-- * Cons, ami egy 'a' típusú értéket és egy NonEmpty a-t összefűz
-- * Last, ami egy darab 'a' típusú elemet tárol
-- ½ pont
-- Ha valaki nem tudja ezt megcsinálni, akkor használja a List a típust helyette az előző óráról!
data NonEmpty a = Cons2 a (NonEmpty a) | Last a
 
-- Írjunk rá Eq instance-ot (deriving semmilyen formában nem használható!) 
-- 1 pont
instance Eq a => Eq (NonEmpty a) where
    (Last a) == (Last b) = a == b
    (Cons2 a as) == (Cons2 b bs) = a == b && as == bs
    _ == _ = False
 
-- Írjunk egy függvényt az alábbi típusszignatúrával, ami típushelyes, totális és nem vezet végtelen ciklushoz!
-- ½ pont
f :: b -> (a -> b) -> Maybe a -> b
f b g (Just a) = g a
f b _ _ = b
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 f g = f (g (\x -> f x x)) (g (\y -> f y y))

-- Ord instance listára
data List a = Nil | Cons a (List a)
infixr 5 `Cons`


-- Szabályok:
-- Ha az egyik fejelem nagyobb, akkor az a lista nagyobb
-- Ha egyenlőek a lista maradéka dönti el
-- Ha az egy lista üres a másik nagyobb

instance Eq a => Eq (List a) where -- = >
    Nil == Nil = True
    (Cons x xs) == (Cons y ys) = x == y && xs == ys
    _ == _ = False

instance Ord a => Ord (List a) where
    Nil <= Nil = True
    (Cons _ _) <= Nil = False
    Nil <= (Cons _ _) = True
    (Cons x xs) <= (Cons y ys) = x <= y && xs <= ys


-- Milyen rekurzív adattípusok vannak még?
-- Pl a bináris fa egy ilyen (Algo 1)

-- Minden levelében van egy érték és minden elágazásban van egy érték
data Tree a = Leaf a | Branch a (Tree a) (Tree a)
-- Csak az elágazásokban van érték
data BranchOnlyTree a = BOLeaf | BoBrnach a (BranchOnlyTree a) (BranchOnlyTree a)
-- Csak a levelekben van érték
data LeafOnlyTree a = LOLeaf a | LOBranch (LeafOnlyTree a) (LeafOnlyTree a)

instance Eq a => Eq (Tree a) where
    (Leaf a) == (Leaf b) = a == b
    (Branch a as as') == (Branch b bs bs') = a == b && as == bs && as' == bs'
    _ == _ = False

instance Eq a => Eq (BranchOnlyTree a) where
    (==) = undefined

instance Eq a => Eq (LeafOnlyTree a) where
    (==) = undefined

-- ezen a tárgyon inkább csak a LeafOnlyTree-t szeretjük használni

treeHeight :: (Ord a, Num a) => Tree b -> a
treeHeight (Leaf a) = 0
treeHeight (Branch a as as') = 1 + max (treeHeight as) (treeHeight as')


-- Listaszerű függvények

treeSum :: Num a => Tree a -> a
treeSum (Leaf a) = a
treeSum (Branch a as as') = a + treeSum as + treeSum as'

boTreeSum :: Num a => BranchOnlyTree a -> a
boTreeSum (BOLeaf) = 0
boTreeSum (BoBrnach a b bs) = a + boTreeSum b + boTreeSum bs

loTreeSum :: Num a => LeafOnlyTree a -> a
loTreeSum (LOLeaf a) = a
loTreeSum (LOBranch a a') = loTreeSum a + loTreeSum a'

treeProduct :: Num a => Tree a -> a
treeProduct (Leaf a) = a
treeProduct (Branch a as as') = a * treeProduct as * treeProduct as'

elemTree :: Eq a => a -> Tree a -> Bool
elemTree a (Leaf b) = a == b
elemTree b (Branch a as as') = a == b || elemTree b as || elemTree b as'

-- hasonlóak ezek a lista fv-ekhez: sum, product és elem
-- lehetne-e az olyan típusokat amelyekre vannak ilyen műveletek generalizálni!

-- Foldable típusosztály: olyan típusokat ír le amelyeket végig lehet hajtogatni

-- Itt elhagytuk az a paramétert, ugyanis ennek minden a típusra működnie kell!
-- GHCi-ben megnézhetjük :i Foldable kiírja, hogya egy (Type -> Type) típust vár

-- Először írjuk meg listára

instance Foldable List where
    foldr _ d Nil = d
    foldr f d (Cons x xs) = f x $ foldr f d xs

instance Foldable Tree where
    foldr f d (Leaf a) = f a d
    foldr f d (Branch a as as') = f a $ foldr f (foldr f d as) as'

-- Foldable-el kapunk menő műveleteket pl sum, minimum, maximum stb

-- Mi lehet még Foldable? Igazából akármi amibe vannak 'a' típusú értékek és nincs 'a' paraméterű függvény
data Maybe' a = Just' a | Nothing'
data NonEmpty' a = a :| [a]

instance Foldable Maybe' where
    foldr f d Nothing' = d
    foldr f d (Just' a) = f a d 

instance Foldable NonEmpty' where
    foldr f d (a :| as) = f a $ foldr f d as