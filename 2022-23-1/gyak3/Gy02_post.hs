{-# options_ghc -Wincomplete-patterns #-}
module Gy02 where

import Prelude hiding (Maybe(..), Either(..), Functor(..))
import Data.List.NonEmpty (NonEmpty)

-- data Either a b = Left a | Right b
f10 :: (a -> c, b -> c) -> Either a b -> c
--f10 (f, g) (Left a) = f a
--f10 (f, g) (Right b) = g b

--f10 (f, g) = either f g

f10 (f, g) = \e -> case e of
  (Left a) -> f a
  (Right b) -> g b

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a,b)) = (a, Left b)
f11 (Right (a,c)) = (a, Right c)

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 (a, Left b) = Left (a,b)
f12 (a, Right c) = Right (a,c)


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".

applyMany :: [a -> b] -> a -> [b]
--applyMany [] x = []
--applyMany (f:fs) x = f x : applyMany fs x
applyMany fs x = map (\f -> f x) fs

-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

--type NonEmptyList a = (a, [a])
--                     (x:xs)
data NonEmptyList a = End a | Extend a (NonEmptyList a)

tesztList :: NonEmptyList Int
tesztList = Extend 5 (Extend 2 (End 4))
--tesztList = End 5
--tesztList = (3, [1,2,3,4]) type-os változat | (3, [])

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

toList :: NonEmptyList Int -> [Int]
toList (End x) = [x]
toList (Extend x n) = x : toList n

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--                         függvények    start  list
composeAll :: [a -> a] -> a -> a
--composeAll [] x = x
--composeAll (f:fs) x = f (composeAll fs x) -- composeAll fs (f x)
--composeAll fs x = (foldr (\f g -> f . g) id fs) x
composeAll = foldr (.) id

compTeszt = [(*3), (+10), (*2)]
-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined


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


-- osztályok
--------------------------------------------------------------------------------

{- Typeclasses: Eq, Ord, Show -}

-- class Show a where
--   show :: a -> String

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (==) a b = not (a /= b)
--   (/=) :: a -> a -> Bool
--   (/=) a b = not (a == b)

-- class Eq a => Ord a where
--   (<=) :: a -> a -> Bool
--   (>=) ...
--   ... (Use `:i Ord` for more info)

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False 

instance Ord Color where
  (<=) Red Green = True
  (<=) Red Blue = True
  (<=) Green Blue = True
  (<=) a b = a == b

instance Show Color where
  show Red = "Piros"
  show Green = "Zold"
  show Blue = "Kek"

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

-- List type + instances

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show = undefined

instance Eq a => Eq (List a) where
  (==) = undefined

instance Ord a => Ord (List a) where
  (<=) = undefined


--- Tree type + instances

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) = undefined

instance Ord a => Ord (Tree a) where
  (<=) = undefined


-- Maybe type + instances

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

instance Eq a => Eq (Maybe a) where
  (==) Nothing Nothing = True
  (==) (Just a) (Just a') = a == a'
  (==) _ _ = False

instance Ord a => Ord (Maybe a) where
  (<=) Nothing Nothing = True
  (<=) Nothing (Just a) = True
  (<=) (Just a) Nothing = False
  (<=) (Just a) (Just a') = a <= a'


-- Either type + instances

data Either a b = Left a  | Right b

-- Here you need to change the instance declarations as well!
instance Show a => Show (Either a b) where
  show = undefined

instance Eq a => Eq (Either a b) where
  (==) = undefined

instance Ord a => Ord (Either a b) where
  (<=) = undefined