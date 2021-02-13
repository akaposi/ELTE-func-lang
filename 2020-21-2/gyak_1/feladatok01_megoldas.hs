{-# options_ghc -Wincomplete-patterns #-}

-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = (/=)


-- függvények
--------------------------------------------------------------------------------

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

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 (Left (a, b))  = (a, Left b)
f10 (Right (a, c)) = (a, Right c)

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 (a, Left b)  = Left (a, b)
f11 (a, Right c) = Right (a, c)

-- bónusz feladat (nehéz)
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 f g = f (g (\a -> f a a)) (g (\a -> f a a))


-- listák
--------------------------------------------------------------------------------

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


-- listával ágazó fa ADT
--------------------------------------------------------------------------------

-- Vegyük a következő ADT-t:
data RTree a = RNode a [RTree a]

-- Írj "Eq a => Eq (RTree a)" instance-t
-- Írj "mapTree :: (a -> b) -> RTree a -> RTree b" függvényt
mapRTree :: (a -> b) -> RTree a -> RTree b
mapRTree f (RNode a ts) = RNode (f a) (map (mapRTree f) ts)


-- Írj "size :: RTree a -> Int" függvényt, ami megszámolja a fában levő
-- "a"-kat. Pl. size (Node 0 [Node 1 []]) == 2
size :: RTree a -> Int
size (RNode a ts) = 1 + sum (map size ts)


-- osztályok
--------------------------------------------------------------------------------

class Eq' a where
  eq :: a -> a -> Bool

class Eq' a => Ord' a where
  lte :: a -> a -> Bool

fun1 :: Ord' a => a -> a -> a -> Bool
fun1 x y z = lte y z

fun2 :: Ord' a => a -> a -> a -> Bool
fun2 x y z = eq x y

class Show' a where
  show' :: a -> String

data Tree a = Leaf a | Node (Tree a) (Tree a)
data Color = Red | Green | Blue

-- írd meg a következő instance-okat
instance Eq' Color where
  eq Red   Red   = True
  eq Green Green = True
  eq Blue  Blue  = True
  eq _     _     = False

instance Ord' Color where
  lte Red   _     = True
  lte Green Green = True
  lte Green Blue  = True
  lte Blue  Blue  = True
  lte _     _     = False

instance Show' Color where
  show' Red   = "Red"
  show' Green = "Green"
  show' Blue  = "Blue"

instance Eq' a => Eq' (Maybe a) where
  eq Nothing  Nothing   = True
  eq (Just a) (Just a') = eq a a'
  eq _        _         = False

instance Ord' a => Ord' (Maybe a) where
  lte Nothing  _         = True
  lte (Just a) (Just a') = lte a a'
  lte _        _         = False

instance Show' a => Show' (Maybe a) where
  show' Nothing  = "Nothing"
  show' (Just a) = "Just " ++ show' a

instance Eq' a => Eq' [a] where
  eq []     []     = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _      _      = False

instance Ord' a => Ord' [a] where
  lte []     _      = True
  lte (x:xs) (y:ys) = lte x y && lte xs ys
  lte _      _      = False

instance Show' a => Show' [a] where
  show' []     = "[]"
  show' (a:as) = "[" ++ show' a ++ concatMap (\a -> ", " ++ show' a) as ++ "]"

instance Eq' a => Eq' (Tree a) where
  eq (Leaf a)   (Leaf a')    = eq a a'
  eq (Node l r) (Node l' r') = eq l l' && eq r r'
  eq _          _            = False

instance Ord' a => Ord' (Tree a) where
  lte (Leaf a)   (Leaf a')    = lte a a'
  lte (Leaf a)   _            = True
  lte (Node l r) (Node l' r') = lte l l' && lte r r'
  lte _          _            = False

instance Show' a => Show' (Tree a) where
  show' (Leaf a)   = "Leaf " ++ show' a
  show' (Node l r) = "Node " ++ par (show' l) ++ " " ++ par (show' r)
    where par str = "(" ++ str ++ ")"
