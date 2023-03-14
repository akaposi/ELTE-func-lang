{-# options_ghc -Wincomplete-patterns #-}
module Gy02 where

import Prelude hiding (Eq(..), Show(..), Ord(..))

-- bónusz feladat (nehéz)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined
applyMany :: [a -> b] -> a -> [b]
-- applyMany [] a = []
-- applyMany (f : fs) a = f a : applyMany fs a
applyMany fs a = map ($ a) fs


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

data NonEmptyList a = End a | Extend a (NonEmptyList a)

ns1 :: NonEmptyList Int
ns1 = Extend 4 (Extend 2 (End 6))
-- ns1 = End 3

type NonEmptyList' a = (a, [a])
ns2 :: NonEmptyList' Int
ns2 = (4 , [])


--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

toList :: NonEmptyList a -> [a]
toList (End a) = [a]
toList (Extend a ns) = a : toList ns

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.

fromList :: [a] -> Maybe (NonEmptyList a)
fromList [] = Nothing
fromList (a : []) = Just (End a)
fromList (a : as) = case fromList as of
  Nothing -> Nothing
  Just nel -> Just (Extend a nel)



-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
-- composeAll [] a = a
-- composeAll (f : fs) a = f $ composeAll fs a
composeAll = foldr (.) id

-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő / monoton növő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge as [] = as
merge [] as = as
merge as'@(a:as) bs'@(b:bs) = if a <= b
  then
    a : merge as bs'
  else
    b : merge as' bs



-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
sublists :: [a] -> [[a]]
sublists = undefined

-- fás feladatok
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

t1 :: Tree Int
t1 = Leaf 100

t2 :: Tree Int
t2 = Node (Leaf 10) (Leaf 20)

-- Számold meg a leveleket.
numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node l r) = numLeaves l + numLeaves r

-- Alkalmazz egy függvényt az összes tárolt értékre
-- Rekurzít típusra rekurzív függvény:
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Összegezd a fában tárolt értékeket.
-- A "Num" osztály metódusa a "(+)" művelet.
sumTree :: Num a => Tree a -> a
sumTree (Leaf a) = a
sumTree (Node l r) = sumTree l + sumTree r

-- osztályok
--------------------------------------------------------------------------------
class Eq a where
  (==) :: a -> a -> Bool    -- (==) metódus, operátorként deklarálva
  infix 4 ==                -- nem asszociatív művelet

class Eq a => Ord a where   -- rendezés
  (<=) :: a -> a -> Bool
  infix 4 <=

class Show a where          -- értékek String-é alakítása
  show :: a -> String

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq Color where
  (==) = undefined

instance Ord Color where
  (<=) = undefined

instance Show Color where
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<=) = undefined

instance Show a => Show (Maybe a) where
  show = undefined

instance Eq a => Eq [a] where
  (==) = undefined

instance Ord a => Ord [a] where
  (<=) = undefined

instance Show a => Show [a] where
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) = undefined

instance Ord a => Ord (Tree a) where
  (<=) = undefined

instance Show a => Show (Tree a) where
  show = undefined

-- Nehezebb feladatok

data CTree a = CLeaf a | CNode (CTree a) a (CTree a)

instance Show a => Show (CTree a) where
  show = undefined

instance Eq a => Eq (CTree a) where
  (==) = undefined

-- lexikografiukus sorrendben
instance Ord a => Ord (CTree a) where
  (<=) = undefined
