{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Maybe monád
--------------------------------------------------------------------------------
{-
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
-}

import Control.Monad

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
mapMaybeTree f (Leaf a) = case f a of
    Nothing -> Nothing
    Just a' -> Just $ Leaf a'
mapMaybeTree f (Node t1 t2 t3) = case mapMaybeTree f t1 of
    Nothing  -> Nothing
    Just t1' -> case mapMaybeTree f t2 of
        Nothing  -> Nothing
        Just t2' -> case mapMaybeTree f t3 of
            Nothing  -> Nothing
            Just t3' -> Just $ Node t1' t2' t3'

-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (x:xs) (y:ys) = case f x y of
    Nothing -> Nothing
    Just c  -> case zipWithMaybe f xs ys of
        Nothing -> Nothing
        Just cs -> Just $ c:cs
zipWithMaybe f _      _      = Just []

-- definiáld újra a függvényeket (>>=) függvény felhasználásával! Ne használj
-- mintaillesztést Maybe típusú értékekre!
{-
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing -> Nothing
    Just as -> if b then Just (a:as) else Just as

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-}
filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe2 f []     = Just []
filterMaybe2 f (a:as) = f a >>= \b -> 
    filterMaybe2 f as >>= \xs -> 
    if b then Just (a:xs) else Just xs

{-
mapMaybeTree f (Leaf a) = case f a of
    Nothing -> Nothing
    Just a' -> Just $ Leaf a'
mapMaybeTree f (Node t1 t2 t3) = case mapMaybeTree f t1 of
    Nothing  -> Nothing
    Just t1' -> case mapMaybeTree f t2 of
        Nothing  -> Nothing
        Just t2' -> case mapMaybeTree f t3 of
            Nothing  -> Nothing
            Just t3' -> Just $ Node t1' t2' t3'
-}
mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 f (Leaf a)        = f a >>= \a' -> Just (Leaf a') -- fmap Leaf $ f a
mapMaybeTree2 f (Node t1 t2 t3) =
    mapMaybeTree2 f t1 >>= \t1' ->
    mapMaybeTree2 f t2 >>= \t2' ->
    mapMaybeTree2 f t3 >>= \t3' -> Just $ Node t1' t2' t3'

{-
zipWithMaybe f (x:xs) (y:ys) = case f x y of
    Nothing -> Nothing
    Just c  -> case zipWithMaybe f xs ys of
        Nothing -> Nothing
        Just cs -> Just $ c:cs
zipWithMaybe f _      _      = Just []
-}
zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 f (x:xs) (y:ys) = 
    f x y >>= \c ->
    zipWithMaybe2 f xs ys >>= \cs ->
    Just $ c:cs
zipWithMaybe2 f _      _      = Just []

-- definiáld újra a függvényeket do notáció használatával!
filterMaybe3 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe3 f []     = Just []
filterMaybe3 f (a:as) = do
    b  <- f a
    xs <- filterMaybe3 f as
    if b then Just (a:xs) else Just xs

{-
mapMaybeTree2 f (Leaf a)        = f a >>= \a' -> Just (Leaf a') -- fmap Leaf $ f a
mapMaybeTree2 f (Node t1 t2 t3) =
    mapMaybeTree2 f t1 >>= \t1' ->
    mapMaybeTree2 f t2 >>= \t2' ->
    mapMaybeTree2 f t3 >>= \t3' -> Just $ Node t1' t2' t3'
-}
mapMaybeTree3 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree3 f (Leaf a)        = do
    a' <- f a
    Just $ Leaf a'
mapMaybeTree3 f (Node t1 t2 t3) = do
    t1' <- mapMaybeTree3 f t1
    t2' <- mapMaybeTree3 f t2
    t3' <- mapMaybeTree3 f t3
    Just $ Node t1' t2' t3'

zipWithMaybe3 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe3 f (x:xs) (y:ys) = do
    b <- f x y
    bs <- zipWithMaybe3 f xs ys
    Just $ b:bs
zipWithMaybe3 f _      _      = Just []


-- IO monád
--------------------------------------------------------------------------------

-- Használjuk a következő standard függvényeket, illetve a do notációt.

-- getLine  :: IO String             -- beolvas egy sort
-- print    :: Show a => a -> IO ()  -- kinyomtat egy értéket amire van Show instance
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- print = putStrLn . show

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- (>>)   :: IO a -> IO b -> IO b -- x >>= \_ -> y
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b

io0 :: IO ()
io0 = putStrLn "Hello world!"

-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a'    és 'z'
-- közötti karakterek számát.
io1 :: IO ()
io1 = do
    str <- getLine
    print (f str)
    -----
    where
        f :: String -> Integer
        f [] = 0
        f (x:xs) | x `elem` ['a'..'z'] = 1 + f xs
                 | otherwise           = f xs


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = do
    str <- getLine
    f (length str) str
    ----
    where
        f :: Int -> String -> IO ()
        f n s
            | n <= 0    = return ()
            | otherwise = do
                putStrLn s
                f (n - 1) s

io2' :: IO ()
io2' = do
    str <- getLine
    replicateM_ (length str) (putStrLn str) 

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c -vel lehet megszakítani a futtatást
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
