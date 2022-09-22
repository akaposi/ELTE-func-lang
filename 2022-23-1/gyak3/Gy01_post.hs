
{-# options_ghc -Wincomplete-patterns #-}
module Gy01_post where

valami :: Int -> Int
valami 5 = 10
valami 20 = 5
valami x = x * 2

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------
-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
--xor b b'
--  | b == b' = False
--  | otherwise = True

--xor True False = True
--xor False True = True
--xor _ _ = False

xor = (/=)

--xor a b = not (a == b)

--xor a b = case a == b of
--  True -> False
--  False -> True

--xor a b = let
--  isSame = a == b
--  isBothTrue = a == True && b == True
--   in
--    not isSame && not isBothTrue 


-- ... where ...

-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (azaz nem lehet végtelen loop
-- vagy kivétel dobás!).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> a -> b
f2 f = f

f3 :: (b -> c) -> (a -> b) -> a -> c
--f3 f g a = f $ g a
f3 f g a = f (g a)

f3' :: (b -> c) -> (a -> b) -> a -> c
--f3' = (.)
f3' f g = f . g

f4 :: (a -> b -> c) -> b -> a -> c
-- f4 f b a = f a b
f4 = flip


f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b) -- curry

--xor' :: (Bool, Bool) -> Bool

-- fst, snd
f6 :: (a -> b -> c) -> (a, b) -> c
--f6 f t = f (fst t) (snd t)
f6 f (a, b) = f a b
--f6 = uncurry

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (fst . f, snd . f)

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) a = (f a , g a)

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 f = (f . Left, f . Right)

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 = undefined

-- Either a b = Left a | Right b
f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 (Left (a, b))= undefined
f11 (Right (a, c)) = undefined

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined

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

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

class Show' a where
  show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq' Color where
  eq = undefined

instance Ord' Color where
  lte = undefined

instance Show' Color where
  show' = undefined

instance Eq' a => Eq' (Maybe a) where
  eq = undefined

instance Ord' a => Ord' (Maybe a) where
  lte = undefined

instance Show' a => Show' (Maybe a) where
  show' = undefined

instance Eq' a => Eq' [a] where
  eq = undefined

instance Ord' a => Ord' [a] where
  lte = undefined

instance Show' a => Show' [a] where
  show' = undefined

instance Eq' a => Eq' (Tree a) where
  eq = undefined

instance Ord' a => Ord' (Tree a) where
  lte = undefined

instance Show' a => Show' (Tree a) where
  show' = undefined
