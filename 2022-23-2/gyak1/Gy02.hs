{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}

module Gy02 where


-- Vegyük az alábbi típust:
data Trie a = TLeaf | TNode a (Trie a) (Trie a) (Trie a) deriving Show

-- Írj rá Eq instance-ot! Deriving semmilyen formában nem használható! (1 pont)
instance Eq a => Eq (Trie a) where
  (==) :: Trie a -> Trie a -> Bool
  TLeaf == TLeaf = True
  (TNode a b c d) == (TNode e f g h) = a == e && b == f && c == g && d == h
  _ == _ = False

-- Definiálj egy függvényt az alábbi típussal! A függvény legyen totális!
-- A megoldásban ne használj már definiált függvényeket (pl undefined)! Függvénykompozíció használható.
-- (1 pont)
f14 :: Either a b -> (a -> c, b -> c) -> c
f14 (Left a) (ac, _) = ac a
f14 (Right b) (_, bc) = bc b

-- Rekurzív adattípusok - olyan típusok ami saját magukkal vannak definiálva
-- Példák:

-- Láncolt Listák
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Bináris Fák
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

-- Bináris Fák (alternatív reprezentáció)
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Eq, Show)

-- Nemüres Listák
data NonEmptyList a = Last a | NECons a (NonEmptyList a) deriving (Eq, Show)


-- Definiáljuk pár függvényt ezekre a típusokra!

-- Adjuk össze az összes elemet
sumList :: Num a => List a -> a
sumList Nil = 0
sumList (Cons a as) = a + sumList as

-- Nézzük meg minden elem igaz-e (használjuk && függvényt!)
allTrueL :: List Bool -> Bool
allTrueL (Cons a as) = a && allTrueL as
allTrueL Nil = True

-- Számoljuk meg mennyi elem van a tárolóban 
lengthList :: List a -> Int
lengthList Nil = 0
lengthList (Cons a as) = 1 + lengthList as

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node l a b) = sumTree l + a + sumTree b

allTrueT :: Tree Bool -> Bool
allTrueT Leaf = True
allTrueT (Node l a r) = allTrueT l && a && allTrueT r

-- Számoljuk meg mennyi 'a' típusú elem van a fában
lengthTree :: Tree a -> Int
lengthTree Leaf = 0
lengthTree (Node l _ r) = 1 + lengthTree l + lengthTree r

sumTree' :: Num a => Tree' a -> a
sumTree' (Leaf' a) = a
sumTree' (Node' xs ys) = sumTree' xs + sumTree' ys

allTrueT' :: Tree' Bool -> Bool
allTrueT' (Leaf' a) = a
allTrueT' (Node' xs ys) = allTrueT' xs && allTrueT' ys

lengthTree' :: Tree' a -> Int
lengthTree' (Leaf' _) = 1
lengthTree' (Node' l r) = lengthTree' l + lengthTree' r

sumNE :: Num a => NonEmptyList a -> a
sumNE (Last a) = a
sumNE (NECons a as) = a + sumNE as

allTrueNE :: NonEmptyList Bool -> Bool
allTrueNE (Last b) = b
allTrueNE (NECons b bs) = b && allTrueNE bs

lengthNE :: NonEmptyList a -> Int
lengthNE (Last _) = 1
lengthNE (NECons _ xs) = 1 + lengthNE xs

-- Ezeknek a műveletknek az implementációja hasonló. Szükséges elemek:
-- Egy termináló érték
-- Egy kétparaméteres függvény ami egy elemből és egy rekurzív hívásból csinál valamit

-- Ennek az általánosítása a hajtogatás
foldrList :: (a -> b -> b) {- függvény-} -> b {- termináló elem-} -> List a -> b
foldrList f b Nil = b
foldrList f b (Cons a as) = f a (foldrList f b as)

-- Ezt annó BSc haskellen csak listákra implementáltuk
-- De mivel ez ha megnézzük ugyanaz mint egy sima rekurzió
-- Lehet implementálni más típusokra is

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f b Leaf = b
foldrTree f b (Node l a r) = foldrTree f (f a (foldrTree f b r)) l

-- A típusnak nem is feltétlenül kell rekurzívnak lennie
-- data Maybe a = Nothing | Just a
foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b
foldrMaybe f b Nothing = b
foldrMaybe f b (Just a) = f a b

-- Lehet-e ezt általánosítani?
-- Érdemes a kifejezések típusát megnézni:

{-

  foldrList  :: (a -> b -> b) -> b -> List  a -> b
  foldrTree  :: (a -> b -> b) -> b -> Tree  a -> b
  foldrMaybe :: (a -> b -> b) -> b -> Maybe a -> b

-}

-- Fő probléma: Nem vehetünk fel polimorf típust pl "List a" helyett
-- mert az 'a' polimorf típus máshol is megjelenik a függvényben

-- Ötlet: Vegyünk fel polimorf típust csak a "List" helyett (nem a "List a") helyett!
-- foldr :: (a -> b -> b) -> b -> f a -> b
-- (f = List vagy Tree vagy Maybe)

-- Ez a koncepció a Magasabbrendű Polimorfizmus
-- Nem konkrét típusoknak vezetünk be polimorf típusparamétert
-- Hanem parciálisan applikált típusoknak

-- GHCi új parancs:
-- :k <Típus>
-- Megmondja mi az úgynevezett kindja egy típusnak

-- :k Int
-- *
-- a csillag az típust jelent
-- :k Maybe
-- * -> *
-- a Maybe vár egy extra típust paraméterül

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr = foldrList

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr = foldrTree

instance Foldable Tree' where
  foldr :: (a -> b -> b) -> b -> Tree' a -> b
  foldr f b (Leaf' a) = f a b
  foldr f b (Node' l r) = foldr f (foldr f b r) l

  foldMap :: Monoid m => (a -> m) -> Tree' a -> m
  foldMap f (Leaf' a) = f a
  foldMap f (Node' l r) = foldMap f l <> foldMap f r

instance Foldable NonEmptyList where
  foldr :: (a -> b -> b) -> b -> NonEmptyList a -> b
  foldr f b (Last a) = f a b
  foldr f b (NECons a as) = f a (foldr f b as)

-- Foldable instance írás algoritmizálható, Haskell tud is magától írni
-- a DeriveFoldable nyelvi kiegészítő bekapcsolásával
-- ezt KisZH-n és Vizsgán nem lehet használni.

-- Jövőheti KisZH Foldable instance írás valami rekurzív datára

-- Alternatív módszer Foldable instance írásra
-- A foldr-el ekvivalens definíció:

{-
foldMap :: Monoid m => (a -> m) -> f a -> m
-}

-- Monoid magyarul: egységelemes félcsoport, az alábbi műveletekkel rendelkezik
-- (<>) :: Monoid m => m -> m -> m | kétparaméteres függvény
-- mempty :: Monoid m => m         | termináló eset

data List' a = Nil' | Cons' a (List' a) deriving (Eq, Show)

instance Foldable List' where
  foldMap :: Monoid m => (a -> m) -> List' a -> m
  foldMap f Nil' = mempty
  foldMap f (Cons' a as) = f a <> foldMap f as

data Tree'' a = Leaf'' | Node'' (Tree'' a) a (Tree'' a) deriving (Eq, Show)

instance Foldable Tree'' where
  foldMap :: Monoid m => (a -> m) -> Tree'' a -> m
  foldMap f Leaf'' = mempty
  foldMap f (Node'' l a r) = foldMap f l <> f a <> foldMap f r
