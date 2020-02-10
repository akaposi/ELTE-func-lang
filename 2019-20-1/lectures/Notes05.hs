
-- Monádok (motivációs feladatok)
------------------------------------------------------------


-- 1. Írj egy függvényt, ami beszámozza balról jobbra 0-tól kezdve
-- egy fa leveleit.

-- Pl: labelTree1 (Node1 (Leaf1 ()) (Node1 (Leaf1 ()) (Leaf1 ())))
--             == (Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2)))

data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

labelTree1 :: Tree1 a -> Tree1 Int
labelTree1 = fst . go 0 where
  go :: Int -> Tree1 a -> (Tree1 Int, Int)
  go n (Leaf1 _)   = (Leaf1 n, n + 1)
  go n (Node1 l r) = case go n l of
    (l, n) -> case go n r of (r, n) -> (Node1 l r, n)

-- ugyanezt a következő fára is:
data Tree2 a = Node2 a [Tree2 a] deriving Show

labelTree2 :: Tree2 a -> Tree2 Int
labelTree2 = fst . go 0 where
  go :: Int -> Tree2 a -> (Tree2 Int, Int)
  go n (Node2 _ ts) = (Node2 n (reverse ts'), n')
    where
      step (ts, n) t = case go n t of (t, n) -> (t:ts, n)
      (ts', n')      = foldl step ([], n + 1) ts

-- pl:  labelTree2 (Node2 () [Node2 () [], Node2 () [Node2 () []]])
--   == (Node2 0 [Node2 1 [], Node2 2 [Node2 3 []]])


-- 2. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing    -> Nothing
  Just True  -> (a:) <$> filterMaybe f as
  Just False -> filterMaybe f as


-- 3. Alkalmazz egy (a -> Maybe b) függvény egy Tree1 minden levelére,
--    ha bármelyik alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
mapMaybeTree f (Leaf1 a) = Leaf1 <$> f a
mapMaybeTree f (Node1 l r) = case mapMaybeTree f l of
  Nothing -> Nothing
  Just l  -> case mapMaybeTree f r of
    Nothing -> Nothing
    Just r  -> Just (Node1 l r)
