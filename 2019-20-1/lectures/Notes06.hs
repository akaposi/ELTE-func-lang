-- Monádok
------------------------------------------------------------

import Control.Monad.State

--  Írd meg a következő függvényt. A függvény úgy működik,
--  mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--  függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--  a végeredmény, egyébként Just <szűrt lista>

-- Használd a Maybe monádot a megoldáshoz!
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = pure []
filterMaybe f (a:as) = do
  b <- f a
  (if b then (a:) else id) <$> filterMaybe f as


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree1 minden levelére,
-- ha bármelyik alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
-- Használd a Maybe monádot!
mapMaybeTree :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
mapMaybeTree f (Leaf1 a)   = Leaf1 <$> f a
mapMaybeTree f (Node1 l r) = do
  l <- mapMaybeTree f l
  r <- mapMaybeTree f r
  pure $ Node1 l r


-- Általánosítsd a filterMaybe függvényt tetszőleges monádra!
-- Szűrd a listát a kapott (a -> m Bool) függvénnyel!
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f []     = pure []
filterM' f (a:as) = do
  b <- f a
  (if b then (a:) else id) <$> filterM' f as


-- Általánosítds a mapMaybeTree függvényt tetszőleges monádra!
mapMTree1 :: Monad m => (a -> m b) -> Tree1 a -> m (Tree1 b)
mapMTree1 f (Leaf1 a)   = Leaf1 <$> f a
mapMTree1 f (Node1 l r) = do
  l <- mapMTree1 f l
  r <- mapMTree1 f r
  pure $ Node1 l r


-- Implementálj stack műveleteket listákra State monádban!

-- adjunk egy értéket a lista elejére
push :: a -> State [a] ()
push a = do
  as <- get
  put (a:as)
-- push a = modify (a:)

-- vegyünk le egy értéket a lista elejéről ha lehetséges,
-- adjunk vissza Just-ot, ha lehetséges, egyébként Nothing-ot.
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> pure Nothing
    a:as -> put as >> pure (Just a)

-- adjunk N-szer egy értéket a lista elejére!
pushNTimes :: Int -> a -> State [a] ()
pushNTimes n a = do
  as <- get
  put $ replicate n a ++ as

-- implementáld általánosan egy monádikus művelet n-szeri végrehajtását!
-- példa: pushNTimes n a = replicateM n (push a)
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma | n <= 0 = pure []
replicateM' n ma = do
  a  <- ma
  as <- replicateM' (n - 1) ma
  pure $ a:as


-- Írj egy függvényt, ami beszámozza balról jobbra 0-tól kezdve
-- egy fa leveleit. Használd a State monádot!

-- Pl: labelTree1 (Node1 (Leaf1 ()) (Node1 (Leaf1 ()) (Leaf1 ())))
--             == (Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2)))
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a) deriving Show

labelTree1 :: Tree1 a -> Tree1 Int
labelTree1 t = evalState (mapMTree1 go t) 0
  where go _ = do {n <- get; put (n + 1); pure n}

-- ugyanezt a következő fára is:
data Tree2 a = Node2 a [Tree2 a] deriving Show

mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f []     = pure []
mapMList f (a:as) = do
  b  <- f a
  bs <- mapMList f as
  pure $ b:bs

mapMTree2 :: Monad m => (a -> m b) -> Tree2 a -> m (Tree2 b)
mapMTree2 f (Node2 a ts) = do
  b  <- f a
  ts <- mapMList (mapMTree2 f) ts
  pure $ Node2 b ts

labelTree2 :: Tree2 a -> Tree2 Int
labelTree2 t = evalState (mapMTree2 go t) 0
  where go _ = do {n <- get; put (n + 1); pure n}


-- pl:  labelTree2 (Node2 () [Node2 () [], Node2 () [Node2 () []]])
--   == (Node2 0 [Node2 1 [], Node2 2 [Node2 3 []]])
