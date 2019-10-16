-- Monádok
------------------------------------------------------------

import Control.Monad.State

--  Írd meg a következő függvényt. A függvény úgy működik,
--  mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--  függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--  a végeredmény, egyébként Just <szűrt lista>

-- Használd a Maybe monádot a megoldáshoz!
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree1 minden levelére,
-- ha bármelyik alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
-- Használd a Maybe monádot!
mapMaybeTree :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
mapMaybeTree = undefined


-- Általánosítsd a filterMaybe függvényt tetszőleges monádra!
-- Szűrd a listát a kapott (a -> m Bool) függvénnyel!
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined


-- Általánosítds a mapMaybeTree függvényt tetszőleges monádra!
mapMTree :: Monad m => (a -> m b) -> Tree1 a -> m (Tree1 b)
mapMTree = undefined


-- Implementálj stack műveleteket listákra State monádban!

-- adjunk egy értéket a lista elejére
push :: a -> State [a] ()
push = undefined

-- vegyünk le egy értéket a lista elejéről ha lehetséges,
-- adjunk vissza Just-ot, ha lehetséges, egyébként Nothing-ot.
pop :: State [a] (Maybe a)
pop = undefined

-- adjunk N-szer egy értéket a lista elejére!
pushNTimes :: Int -> a -> State [a] ()
pushNTimes = undefined


-- implementáld általánosan egy monádikus művelet n-szeri végrehajtását!
-- példa: pushNTimes n a = replicateM n (push a)
replicateM :: Monad m => Int -> m a -> m a
replicateM = undefined


-- Írj egy függvényt, ami beszámozza balról jobbra 0-tól kezdve
-- egy fa leveleit. Használd a State monádot!

-- Pl: labelTree1 (Node1 (Leaf1 ()) (Node1 (Leaf1 ()) (Leaf1 ())))
--             == (Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2)))
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

labelTree1 :: Tree1 a -> Tree1 Int
labelTree1 = undefined
  where
    -- használd a következő segédfüggvényt!
    go :: Tree1 a -> State Int (Tree1 Int)
    go = undefined


-- ugyanezt a következő fára is:
data Tree2 a = Node2 a [Tree2 a] deriving Show

labelTree2 :: Tree2 a -> Tree2 Int
labelTree2 = undefined
  where
    go :: Tree2 a -> State Int (Tree2 Int)
    go = undefined


-- pl:  labelTree2 (Node2 () [Node2 () [], Node2 () [Node2 () []]])
--   == (Node2 0 [Node2 1 [], Node2 2 [Node2 3 []]])
