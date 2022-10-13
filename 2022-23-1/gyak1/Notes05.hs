
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Maybe monád
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Eq, Show)

-- Írd meg a következő függvényt. A függvény úgy működik, mint a lista "filter",
-- viszont ha a kapott (a -> Maybe Bool) függvény valamely elemre Nothing-ot ad,
-- akkor Nothing legyen a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing -> Nothing
    Just as -> if b then Just (a:as) else Just as

  -- Nothing -> Nothing
  -- Just b  -> case filterMaybe f as of
  --   Nothing -> Nothing
  --   Just as -> Just (if b then (a:as) else as)

-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden levelére, ha bármelyik
-- alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined

-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined


-- definiáld újra a függvényeket (>>=) függvény felhasználásával! Ne használj
-- mintaillesztést Maybe típusú értékekre!
filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe2 = undefined

mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 = undefined

zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 = undefined


-- definiáld újra a függvényeket do notáció használatával!
filterMaybe3 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe3 = undefined

mapMaybeTree3 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree3 = undefined

zipWithMaybe3 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe3 = undefined


-- IO monád
--------------------------------------------------------------------------------

-- Használjuk a következő standard függvényeket, illetve a do notációt.

-- getLine  :: IO String             -- beolvas egy sort
-- print    :: Show a => a -> IO ()  -- kinyomtat egy értéket amire van Show instance
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- (>>)   :: IO a -> IO b -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a'    és 'z'
-- közötti karakterek számát.
io1 :: IO ()
io1 = undefined


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = undefined

--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.

f1 :: Monad m => (a -> b) -> m a -> m b -- fmap nélkül definiáld!
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

--------------------------------------------------------------------------------
