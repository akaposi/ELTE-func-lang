
-- Függvények ismétlés, ADT-k
--------------------------------------------------------------------------------

-- 1. Definiáld a következő függvényeket.

f1 :: (a -> (b, c)) -> (a -> b, a -> c)
f1 = undefined

f2 :: (a -> b, a -> c) -> (a -> (b, c))
f2 = undefined

f3 :: (Either a b -> c) -> (a -> c, b -> c)
f3 = undefined

f4 :: (a -> c, b -> c) -> (Either a b -> c)
f4 = undefined

-- bónusz feladat
f5 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f5 = undefined

-- 2. Definiáld a "partition :: (a -> Bool) -> [a] -> ([a], [a])" függvényt, ami
--    az első output listában visszaadja azokat az elemeket, amelyekre az "f :: a -> Bool"
--    igaz, a második output-ban pedig azokat, amire "f" hamis.

-- 3. Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
--    az összes bemenő függvény kompozíciója,
--    pl. "composeAll [f, g, h] x == f (g (h x))"

-- 4. Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
--    rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.

-- 5. (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
--     iterált felhasználásával rendez egy listát.

-- 6. (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
--    minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
--    [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
--    fontos, hogy az össze részlista szerepeljen.

-- 7. Vegyük a következő ADT-t:

data Tree a = Node a [Tree a]

--    Írj "Eq a => Eq (Tree a)" instance-t
--    Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt

-- 8. Vegyük a következő ADT-t:

data Tree2 a = Leaf a | Brach (Int -> Tree2 a)

--    Írj legalább 5 darab (Tree2 a) típusú definíciót.
--    Írj "mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b" függvényt.

-- 9. Definiáld a következő függvényt:

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = undefined

--    Működés: alkalmazzuk a kapott függvényt a lista minden elemére,
--    ha minden függvényalkalmazás eredménye Just, akkor a végeredmény
--    legyen (Just <az összes b-típusú eredmény listája>), egyébként Nothing.
