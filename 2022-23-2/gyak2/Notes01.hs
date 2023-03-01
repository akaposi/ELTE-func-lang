
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

--------------------------------------------------------------------------------


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
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> b -> a -> c
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> (b -> c))
f5 = undefined

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
