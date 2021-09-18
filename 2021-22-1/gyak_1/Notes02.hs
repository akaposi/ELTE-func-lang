
-- köv feladat: 16:00-16:15-ig beküldés
--   .hs fájlt canvas-ba feltölteni
--   undefined definíciót kitölteni
--   néhány teszteset mellékelve

-- tartalom: ismétlőfeladat:
--    listák + magasabbrendű függvények

-- feladatok: javaslat: - nehéz feladatot átugrani
--                      - önálló haladás feladatsorban

-- lambda.inf.elte.hu kezdő Haskell

-- listák
--------------------------------------------------------------------------------

-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
-- applyMany :: [a -> b] -> a -> [b]
-- applyMany = undefined

-- applyMany [f, g, h] a = [f a, g a, h a]

-- rekurzív definíció
applyMany :: [a -> b] -> a -> [b]
applyMany []     a = []
applyMany (f:fs) a = f a : applyMany fs a

applyMany' :: [a -> b] -> a -> [b]
applyMany' fs a = map (\f -> f a) fs

-- ($) :: (a -> b) -> a -> b   -- függvényalkalmazás mint operátor
applyMany'' :: [a -> b] -> a -> [b]
applyMany'' fs a = map ($ a) fs

-- map (+10) [0..10]
-- map (10+) [0..10]


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.

--   - Írj egy "toList :: NonEmptyList a -> [a]" függvényt!

--   - Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--     nemüres listát ad vissza egy standard listából, ha az input nem
--     üres.

-- 2 természetes megoldás

-- data List a = Nil | Cons a (List a)
data NonEmptyList a = Single a | Cons a (NonEmptyList a)

toList :: NonEmptyList a -> [a]
toList (Single a)  = [a]
toList (Cons a as) = a : toList as

-- "case" kifejezés használata
fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (a:as) = case fromList as of
  Nothing -> Just (Single a)
  Just as -> Just (Cons a as)

-- -- "case" kifejezés használata
-- fromList :: [a] -> Maybe (NonEmptyList a)
-- fromList []     = Nothing
-- fromList (a:as) = Just (case fromList as of
--   Nothing -> Single a
--   Just as -> Cons a as)

type NonEmptyList' a = (a, [a])

toList' :: NonEmptyList' a -> [a]
toList' (a, as) = a:as

fromList' :: [a] -> Maybe (NonEmptyList' a)
fromList' []     = Nothing
fromList' (a:as) = Just (a, as)

-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"

-- rekurzív def.
-- O(N) stack
composeAll :: [a -> a] -> a -> a
composeAll []     a = a
composeAll (f:fs) a = f (composeAll fs a)

-- fordított sorrendben alkalmazás (vég-rekurzív)
-- (akkumulátoros rekurzió)
-- O(1) stack
composeAll' :: [a -> a] -> a -> a
composeAll' []     a = a
composeAll' (f:fs) a = composeAll' fs (f a)

-- fold függvények! foldr, foldl

-- composeAll' [f, g, h] a = f (g (h a)) = (f . g . h) a
-- composeAll' [] a = a = id a
composeAll'' :: [a -> a] -> a -> a
composeAll'' fs a = foldr (.) id fs a
   -- composeAll'' fs = foldr (.) id fs

-- foldr f z [a, b, c] = f a (f b (f c z))
-- foldr (.) id [a, b, c] = (.) a ((.) b ((.) c id))
--                        = a . (b . (c . id))
--                        = a . b . c . id
--                        = a (b (c (id x)))
--                        = a (b (c x))

composeAll''' :: [a -> a] -> a -> a
composeAll''' fs a = foldl (flip (.)) id fs a
-- extra házi: megnézni, hogy hogyan fejtődik ki a definíció


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge xs ys = xs ++ ys


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort as = mergeAll (map (\a -> [a]) as) where

  mergeAll []   = []
  mergeAll [as] = as
  mergeAll ass  = mergeAll (mergePairs ass)

  mergePairs (as1:as2:ass) = merge as1 as2 : mergePairs ass
  mergePairs ass           = ass

  -- Data.List.sort: fenti definíció + kis optimalizálás

-- 1. 1-elemű listákat képezünk
--    páronként merge-öljük amíg 1 lista marad




-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
-- Kapcsolódó fogalom: hatványhalmaz
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (a:as) =
  let res = sublists as in res ++ map (a:) res
  -- nem hatékony: sublists as ++ map (a:) (sublists as)

-- bónusz feladat: definiáld a foldl függvényt listára úgy, hogy csak foldr-t és lambda kifejezést
-- használhatsz (rekurziót sem!)


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
