
-- Függvények ismétlés, ADT-k
--------------------------------------------------------------------------------

-- 1. Definiáld a következő függvényeket.

f1 :: (a -> (b, c)) -> (a -> b, a -> c)
f1 f = (fst . f, snd . f)

f2 :: (a -> b, a -> c) -> (a -> (b, c))
f2 (f, g) a = (f a, g a)

f3 :: (Either a b -> c) -> (a -> c, b -> c)
f3 f = (f . Left, f . Right)

f4 :: (a -> c, b -> c) -> (Either a b -> c)
f4 (f, g) (Left a)  = f a
f4 (f, g) (Right b) = g b

-- bónusz feladat
f5 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f5 f g = f (g (\a -> f a a)) (g (\a -> f a a))

-- 2. Definiáld a "partition :: (a -> Bool) -> [a] -> ([a], [a])" függvényt, ami
--    az első output listában visszaadja azokat az elemeket, amelyekre az "f :: a -> Bool"
--    igaz, a második output-ban pedig azokat, amire "f" hamis.

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f as = (filter f as, filter (not . f) as)

-- 3. Definiáld a "composeAll :: [a -> a] -> a -> a" függvényt. Az eredmény legyen
--    az összes bemenő függvény kompozíciója,
--    pl. "composeAll [f, g, h] x == f (g (h x))"
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

-- 4. Definiáld a "merge :: Ord a => [a] -> [a] -> [a]" függvényt, ami két nemcsökkenő
--    rendezett listát összefésül úgy, hogy az eredmény is rendezett maradjon.
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge xs ys = xs ++ ys

-- 5. (bónusz) Definiáld a "mergeSort :: Ord a => [a] -> [a]" függvényt, ami a "merge"
--     iterált felhasználásával rendez egy listát.
mergeSort :: Ord a => [a] -> [a]
mergeSort = mergeAll . map (\x -> [x]) where
  mergePairs (xs:ys:yss) = merge xs ys : mergePairs yss
  mergePairs yss         = yss

  mergeAll [as] = as
  mergeAll as   = mergeAll (mergePairs as)


-- 6. (bónusz) Definiáld a "sublists :: [a] -> [[a]]" függvényt, ami a bemenő lista
--    minden lehetséges részlistáját visszaadja. Pl. "sublists [1, 2] == [[],
--    [1], [2], [1, 2]]".  A részlisták sorrendje az eredményben tetszőleges, a
--    fontos, hogy az össze részlista szerepeljen.
sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (a:as) = let as' = sublists as in map (a:) as' ++ as'

-- 7. Vegyük a következő ADT-t:

data Tree a = Node a [Tree a]

--    Írj "Eq a => Eq (Tree a)" instance-t
--    Írj "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node a ts) = Node (f a) (map (mapTree f) ts)

-- 8. Vegyük a következő ADT-t:

data Tree2 a = Leaf a | Branch (Int -> Tree2 a)

--    Írj legalább 5 darab (Tree2 a) típusú definíciót.
--    Írj "mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b" függvényt.
t1 = Leaf 0
t2 = Branch $ \i -> Leaf (i + 1)
t3 = Branch $ \i -> Branch $ \j -> Leaf (i + j)
t4 = Branch $ \i -> if i < 0 then Leaf "foo" else Leaf "bar"
t5 = Branch $ \_ -> t2

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 f (Leaf a)   = Leaf (f a)
mapTree2 f (Branch g) = Branch (\i -> mapTree2 f (g i))

-- 9. Definiáld a következő függvényt:
--    Működés: alkalmazzuk a kapott függvényt a lista minden elemére,
--    ha minden függvényalkalmazás eredménye Just, akkor a végeredmény
--    legyen (Just <az összes b-típusú eredmény listája>), egyébként Nothing.

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just a) f = f a

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) =
  bindMaybe (f a) $ \a ->
  bindMaybe (mapMaybe f as) $ \as ->
  Just (a : as)
