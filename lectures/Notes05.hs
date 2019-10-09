
-- Monádok
------------------------------------------------------------


-- 1. Írj egy függvényt, ami beszámozza balról jobbra 0-tól kezdve
-- egy fa leveleit.

-- Pl: labelTree1 (Node1 (Leaf1 ()) (Node1 (Leaf1 ()) (Leaf1 ())))
--             == (Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2)))

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

labelTree1 :: Tree1 a -> Tree1 Int
labelTree1 = undefined


-- ugyanezt a következő fára is:
data Tree2 a = Node2 a [Tree2 a] deriving Show

labelTree2 :: Tree2 a -> Tree2 Int
labelTree2 = undefined


-- pl:  labelTree2 (Node2 () [Node2 () [], Node2 () [Node2 () []]])
--   == (Node2 0 [Node2 1 [], Node2 2 [Node2 3 []]])



-- 2. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined


-- 3. Alkalmazz egy (a -> Maybe b) függvény egy Tree1 minden levelére,
--    ha bármelyik alkalmazás Nothing-ot ad, legyen az eredmény Nothing!

mapMaybeTree :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
mapMaybeTree = undefined
