
-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = (/=)


-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (nem lehet végtelen loop
-- vagy exception).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

f2 :: (a -> b) -> a -> b
f2 f a = f a  -- f2 = id

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)  -- f3 = (.)

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 f b a = f a b  -- f4 = flip

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 f a b = f (a, b)  -- f5 = curry

f6 :: (a -> (b, c)) -> (a -> b, a -> c)
f6 f = (fst . f, snd . f)

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) a = (f a, g a)

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (f . Left, f . Right)

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f, g) (Left a)  = f a
f9 (f, g) (Right b) = g b

-- bónusz feladat (nehéz)
f10 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f10 f g = f (g (\a -> f a a)) (g (\a -> f a a))


-- Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
-- listában található minden függvényt alkalmaz egy
-- értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
applyMany :: [a -> b] -> a -> [b]
applyMany fs a = map (\f -> f a) fs


-- Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
-- típusszinonímaként, aminek az értékei nemüres listák.
type NonEmptyList a = (a, [a])

-- Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
-- nemüres listát ad vissza egy standard listából, ha az input nem
-- üres.
fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (x:xs) = Just (x, xs)

--    Írj egy "toList :: NonEmptyList a -> [a]" függvényt, ami értelemszerűen
--    működik
toList :: NonEmptyList a -> [a]
toList (x, xs) = x : xs


-- Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
-- az összes bemenő függvény kompozíciója,
-- pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id


-- Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
-- rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge xs ys = xs ++ ys


-- (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
-- iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = mergeAll . map (\x -> [x]) where
  mergePairs (xs:ys:yss) = merge xs ys : mergePairs yss
  mergePairs yss         = yss

  mergeAll [as] = as
  mergeAll as   = mergeAll (mergePairs as)



-- (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
-- minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
-- [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
-- fontos, hogy az össze részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (a:as) = let as' = sublists as in map (a:) as' ++ as'


-- Vegyük a következő ADT-t:
data Tree a = Node a [Tree a]

-- Írj "Eq a => Eq (Tree a)" instance-t
-- Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node a ts) = Node (f a) (map (mapTree f) ts)

-- Írj "size :: Tree a -> Int" függvényt, ami megszámolja a fában levő
-- "a"-kat. Pl. size (Node 0 [Node 1 []]) == 2
size :: Tree a -> Int
size (Node a ts) = 1 + sum (map size ts)
