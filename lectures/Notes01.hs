
-- Magasabbrendű függvények, ADT-k (ismétlés)
--------------------------------------------------------------------------------

-- ajánlott online jegyzetek:
--   http://lambda.inf.elte.hu/
--     - Kezdő Haskell szekció: magasabbrendű függvények
--     - Haladó Haskell szekció: típusdefiníciók

--------------------------------------------------------------------------------

-- 1. Definiáld a "xor" műveletet Bool típuson.
xor :: Bool -> Bool -> Bool
xor = undefined


-- 2. Írj egy függvényt, ami megadja az első n darab négyzetszám összegét.
--    Példa: sqrsum 10 == 285. Tipp: listakifejezést érdemes használni,
--    lásd: http://lambda.inf.elte.hu/Comprehensions.xml
sqrSum :: Int -> Int   -- sum of first n square numbers
sqrSum n = undefined


-- 3. Definiáld a következő függvényeket tetszőlegesen, de
--    típushelyesen és totális függvényként (nem lehet végtelen loop
--    vagy kivétel).
f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = undefined


-- 4. Definiáld újra a lista típust ADT-ként, "List a" néven.  Legyen
-- két konstruktor, egy az üres listáknak, egy pedig a kiegészített
-- listáknak

-- Írj egy
--    "mapList :: (a -> b) -> List a -> List b", ami a lista minden
--    elemére egy függvényt alkalmaz.
data List a  -- egészítsd ki konstruktorokkal

mapList :: (a -> b) -> List a -> List b
mapList = undefined  -- add meg a definíciót


-- 5. Definiálj egy "BinTree" típust, aminek csak annotáció nélküli
--    levelei és bináris elágazásai vannak.  Írj egy "numLeaves ::
--    BinTree -> Int" függvényt, ami megszámolja a leveleket.
data BinTree

numLeaves :: BinTree -> Int
numLeaves = undefined


-- 6. Írj egy "mapTree :: (a -> b) -> Tree a -> Tree b" függvényt, ami
--    az elágazásokban levő "a" értékekre egy függvényt alkalmaz.
data Tree a = TLeaf | TNode a (Tree a) (Tree a)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined


-- 7. Írj egy "applyMany :: [a -> b] -> a -> [b]" függvényt, ami egy
--    listában található minden függvényt alkalmaz egy
--    értékre. Pl. "applyMany [(+10), (*10)] 10 == [20, 100]".
applyMany :: [a -> b] -> a -> [b]
applyMany = undefined


-- 8. Definiálj egy "NonEmptyList a" típust, akár ADT-ként, akár
--    típusszinonímaként, aminek az értékei nemüres listák.
--
--    Írj egy "fromList :: [a] -> Maybe (NonEmptyList a)" függvényt, ami
--    nemüres listát ad vissza egy standard listából, ha az input nem
--    üres.
data NonEmptyList a -- lehet "type NonEmptyList a = ..." is

fromList :: [a] -> Maybe (NonEmptyList a)
fromList = undefined

--    Írj egy "toList :: NonEmptyList a -> [a]" függvényt, ami értelemszerűen
--    működik
toList :: NonEmptyList a -> [a]
toList = undefined
