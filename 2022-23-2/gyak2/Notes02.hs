
{-# language InstanceSigs, StandaloneDeriving, UndecidableInstances #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést, vagy
-- Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined


-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen és totális
-- függvényként (azaz nem lehet végtelen rekurzió vagy kivétel dobás!).

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> (a -> b)
f2 = id

f2' :: (a -> b) -> (a -> b)
f2' = ($)                        -- f (g (h x))

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = (.)

f4 :: (a -> b -> c) -> b -> a -> c
f4 = flip

f5 :: ((a, b) -> c) -> a -> b -> c
f5 f a b = f (a, b)

f6 :: (a -> b -> c) -> (a, b) -> c
f6 = undefined

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 = undefined

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 = undefined

f9 :: (Either a b -> c) -> (a -> c, b -> c)
f9 = undefined

f10 :: (a -> c, b -> c) -> (Either a b -> c)
f10 = undefined

f11 :: Either (a, b) (a, c) -> (a, Either b c)
f11 = undefined

f12 :: (a, Either b c) -> Either (a, b) (a, c)
f12 = undefined

-- (bónusz, nehezebb)
f13 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f13 = undefined


-- Listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre.
-- Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".

applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- Definiálj egy "NonEmptyList a" típust "data"-ként.
-- aminek az értékei nemüres listák.

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = undefined

-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl:
--   sublists [1, 2] == [[], [1], [2], [1, 2]]
--   sublists "abc" == ["","a","b","c","ab","ac","bc","abc"]
-- A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az összes részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists = undefined


-- osztályok
--------------------------------------------------------------------------------

data Color = Red | Green | Blue
data List a = Nil | Cons a (List a)
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- írd meg a következő instance-okat!

instance Eq Color where
  (==) :: Color -> Color -> Bool
  (==) = undefined

instance Ord Color where
  (<=) :: Color -> Color -> Bool
  (<=)  = undefined

instance Show Color where
  show :: Color -> String
  show  = undefined

instance Eq a => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) = undefined

instance Show a => Show (List a) where
  show :: List a -> String
  show = undefined

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

instance Show a => Show (Tree a) where
  show :: Tree a -> String
  show = undefined



-- Functor
--------------------------------------------------------------------------------

-- írd meg a következő instance-okat!

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap = undefined

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap = undefined

data Twice a = Twice a a deriving (Eq, Show)

instance Functor Twice where
  fmap :: (a -> b) -> Twice a -> Twice b
  fmap = undefined

data Triple a = Triple a a deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap = undefined

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair c) where
  fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap = undefined


-- Bónusz feladatok
--------------------------------------------------------------------------------

-- Add meg a következő definíciókat típushelyesen,
-- végtelen loop és kivételek nélkül!

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

data Product f g a = Product (f a) (g a) deriving Show

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined

newtype Exp x f a = Exp (x -> f a)

instance Functor f => Functor (Exp x f) where
  fmap = undefined

-- Mire használható ez a függvény? Tipp: a megoldáshoz rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

newtype Fix f = Fix (f (Fix f))

-- A "StandaloneDeriving" opciót használjuk itt, mert
-- a standard "deriving Show" már nem működik.
deriving instance Show (f (Fix f)) => Show (Fix f)

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
