{-# options_ghc -Wincomplete-patterns #-}
module Gy02 where

import Prelude hiding (Maybe(..), Either(..), Functor(..))

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 = undefined

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a,b)) = undefined
f11 (Right (a,c)) = undefined

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined


-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

applyMany :: [a -> b] -> a -> [b]
applyMany = undefined

-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.


--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"

composeAll :: [a -> a] -> a -> a
composeAll = undefined


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
  (==) = undefined

instance Ord Color where
  (<=) = undefined

instance Show Color where
  show = undefined

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
  show = undefined

instance Eq a => Eq (Maybe a) where
  (==) = undefined

instance Ord a => Ord (Maybe a) where
  (<=) = undefined


-- Either type + instances

data Either a b = Left a  | Right b

-- Here you need to change the instance declarations as well!
instance Show a => Show (Either a b) where
  show = undefined

instance Eq a => Eq (Either a b) where
  (==) = undefined

instance Ord a => Ord (Either a b) where
  (<=) = undefined