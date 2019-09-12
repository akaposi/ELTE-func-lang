
-- Magasabbrendű függvények, ADT-k (ismétlés)
--------------------------------------------------------------------------------

-- ajánlott online jegyzetek:
--   http://lambda.inf.elte.hu/
--     - Kezdő Haskell szekció: magasabbrendű függvények
--     - Haladó Haskell szekció: típusdefiníciók

--------------------------------------------------------------------------------

-- 1. Definiáld a "xor" műveletet Bool típuson.
xor :: Bool -> Bool -> Bool
xor a b = a /= b


-- 2. Írj egy függvényt, ami megadja az első n darab négyzetszám összegét.
--    Példa: sqrsum 10 == 285. Tipp: listakifejezést érdemes használni,
--    lásd: http://lambda.inf.elte.hu/Comprehensions.xml
sqrSum :: Int -> Int   -- sum of first n square numbers
sqrSum n = sum [x*x | x <- [0..n-1]]


-- 3. Definiáld a következő függvényeket tetszőlegesen, de
--    típushelyesen és totális függvényként (nem lehet végtelen loop
--    vagy kivétel).
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


-- 4. Definiáld újra a lista típust ADT-ként, "List a" néven.  Legyen
-- két konstruktor, egy az üres listáknak, egy pedig a kiegészített
-- listáknak

-- Írj egy
--    "mapList :: (a -> b) -> List a -> List b", ami a lista minden
--    elemére egy függvényt alkalmaz.
data List a = Nil | Cons a (List a)

mapList :: (a -> b) -> List a -> List b
mapList f Nil         = Nil
mapList f (Cons a as) = Cons (f a) (mapList f as)


-- 5. Definiálj egy "BinTree" típust, aminek csak annotáció nélküli
--    levelei és bináris elágazásai vannak.  Írj egy "numLeaves ::
--    BinTree -> Int" függvényt, ami megszámolja a leveleket.
data BinTree = BTLeaf | BTNode BinTree BinTree

numLeaves :: BinTree -> Int
numLeaves BTLeaf       = 0
numLeaves (BTNode l r) = numLeaves l + numLeaves r


-- 6. Írj egy "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt, ami
--    az elágazásokban levő "a" értékekre egy függvényt alkalmaz.
data Tree a = TLeaf | TNode a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f TLeaf         = TLeaf
mapTree f (TNode a l r) = TNode (f a) (mapTree f l) (mapTree f r)


-- 7. Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
--    listában található minden függvényt alkalmaz egy
--    értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
applyMany :: [a -> b] -> a -> [b]
applyMany fs a = map (\f -> f a) fs


-- 8. Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
--    típusszinonímaként, aminek az értékei nemüres listák.
--
--    Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--    nemüres listát ad vissza egy standard listából, ha az input nem
--    üres.
type NonEmptyList a = (a, [a])

fromList :: [a] -> Maybe (NonEmptyList a)
fromList []     = Nothing
fromList (x:xs) = Just (x, xs)

--    Írj egy "toList :: NonEmptyList a -> [a]" függvényt, ami értelemszerűen
--    működik
toList :: NonEmptyList a -> [a]
toList (x, xs) = x : xs
