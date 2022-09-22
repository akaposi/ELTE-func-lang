{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Ora2 where

f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined

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
    (<=) = undefined


-- Milyen rekurzív adattípusok vannak még?
-- Pl a bináris fa egy ilyen (Algo 1)

-- Minden levelében van egy érték és minden elágazásban van egy érték
data Tree a
-- Csak az elágazásokban van érték
data BranchOnlyTree a
-- Csak a levelekben van érték
data LeafOnlyTree a

instance Eq a => Eq (Tree a) where
    (==) = undefined

instance Eq a => Eq (BranchOnlyTree a) where
    (==) = undefined

instance Eq a => Eq (LeafOnlyTree a) where
    (==) = undefined

-- ezen a tárgyon inkább csak a LeafOnlyTree-t szeretjük használni

treeHeight :: Num a => Tree b -> a
treeHeight = undefined


-- Listaszerű függvények

treeSum :: Num a => Tree a -> a
treeSum = undefined

boTreeSum :: Num a => BranchOnlyTree a -> a
boTreeSum = undefined

loTreeSum :: Num a => LeafOnlyTree a -> a
loTreeSum = undefined

treeProduct :: Num a => Tree a -> a
treeProduct = undefined

elemTree :: Eq a => a -> Tree a -> Bool
elemTree = undefined

-- hasonlóak ezek a lista fv-ekhez: sum, product és elem
-- lehetne-e az olyan típusokat amelyekre vannak ilyen műveletek generalizálni!

-- Foldable típusosztály: olyan típusokat ír le amelyeket végig lehet hajtogatni

-- Itt elhagytuk az a paramétert, ugyanis ennek minden a típusra működnie kell!
-- GHCi-ben megnézhetjük :i Foldable kiírja, hogya egy (Type -> Type) típust vár

-- Először írjuk meg listára

instance Foldable List where
    foldr = undefined

instance Foldable Tree where
    foldr = undefined

-- Foldable-el kapunk menő műveleteket pl sum, minimum, maximum stb

-- Mi lehet még Foldable? Igazából akármi amibe vannak 'a' típusú értékek és nincs 'a' paraméterű függvény
data Maybe' a = Just a | Nothing
data NonEmpty' a = a :| [a]

instance Foldable Maybe' where
    foldr = undefined

instance Foldable NonEmpty' where
    foldr = undefined
