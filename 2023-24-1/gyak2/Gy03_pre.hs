{-# OPTIONS_GHC -Wincomplete-patterns -Wnoncanonical-monad-instances -Wname-shadowing #-}
{-# LANGUAGE InstanceSigs, LambdaCase, MultiWayIf #-}

module Gy02 where


data AType a = C1 a | C2 a a | C3 a Int | C4 (AType a) deriving (Eq, Show)
data FList f a = FNill | FCons (f a) (FList f a) deriving (Eq, Show)

instance Functor AType where
  fmap f (C1 a) = C1 (f a)
  fmap f (C2 a b) = C2 (f a) (f b)
  fmap f (C3 a i) = C3 (f a) i
  fmap f (C4 fa) = C4 (fmap f fa)

instance Functor f => Functor (FList f) where
  fmap :: (a -> b) -> FList f a -> FList f b
  fmap f FNill = FNill
  fmap f (FCons fa flist) = FCons (fmap f fa) (fmap f flist)

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
sumList = undefined

-- Nézzük meg minden elem igaz-e (használjuk && függvényt!)
allTrueL :: List Bool -> Bool
allTrueL = undefined

-- Számoljuk meg mennyi elem van a tárolóban 
lengthList :: List a -> Int
lengthList = undefined

sumTree :: Num a => Tree a -> a
sumTree = undefined

allTrueT :: Tree Bool -> Bool
allTrueT = undefined

-- Számoljuk meg mennyi 'a' típusú elem van a fában
lengthTree :: Tree a -> Int
lengthTree = undefined

sumTree' :: Num a => Tree' a
sumTree' = undefined

allTrueT' :: Tree' Bool -> Bool
allTrueT' = undefined

lengthTree' :: Tree' a -> Int
lengthTree' = undefined

sumNE :: Num a => NonEmptyList a -> a
sumNE = undefined

allTrueNE :: NonEmptyList Bool -> Bool
allTrueNE = undefined

lengthNE :: NonEmptyList a -> Int
lengthNE = undefined

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
foldrTree f b (Node tr a tr') = foldrTree f (f a (foldrTree f b tr')) tr

-- A típusnak nem is feltétlenül kell rekurzívnak lennie
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

instance Foldable NonEmptyList where
  foldr :: (a -> b -> b) -> b -> NonEmptyList a -> b
  foldr = undefined

data SkipList a = Skip (SkipList a) | SCons a (SkipList a) | SNill deriving (Eq, Show)
data CrazyType a = C1 a a | C2 a Int | C3 (CrazyType a) deriving (Eq, Show)
data Either3 a b c = Left3 a | Middle3 b | Right3 c deriving (Eq, Show)
data Triplet a b c = Triplet a b c deriving (Eq, Show)

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
  foldMap = undefined

data Tree'' a = Leaf'' | Node'' (Tree'' a) a (Tree'' a) deriving (Eq, Show)

instance Foldable Tree'' where
  foldMap :: Monoid m => (a -> m) -> Tree'' a -> m
  foldMap = undefined
