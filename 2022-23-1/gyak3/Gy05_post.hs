{-# options_ghc -Wincomplete-patterns #-}

module Gy05 where
import Control.Monad

-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd kiírja a sorban található
-- 'a' betűk számát.
io1 :: IO ()
io1 = do
    line <- getLine
    let number = length (filter (=='a') line)
    print number


printNTimes :: String -> Int -> IO ()
printNTimes str 0 = return () 
printNTimes str x = do
    putStrLn str
    printNTimes str (x - 1)

-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = do
    line <- getLine
    putStrLn "------"
    let len = length line
    printNTimes line len

io2' :: IO ()
io2' = do
    line <- getLine
    putStrLn "------"
    replicateM_ (length line) (putStrLn line)



-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert.
io3 :: IO ()
io3 = do
    line <- getLine
    if elem 'x' line then do return () else io3 
-- elem

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3' :: IO ()
io3' = go [] where
    go :: [String] -> IO ()
    go lines = do
        line <- getLine
        if elem 'x' line then do
            mapM_ putStrLn (reverse lines)
            putStrLn line
        else go (line : lines)

io3'' :: IO ()
io3'' = do
    line <- getLine
    if elem 'x' line then return () else io3''
    putStrLn line
    


countLowerCase :: String -> Int
countLowerCase s = length (filter (\c -> elem c ['a'..'z']) s) 

-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = do
    line <- getLine
    print (countLowerCase line)
    io4

--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.

f1 :: Monad m => (a -> b) -> m a -> m b
--f1 = fmap
--f1 = (<$>)
f1 = liftM

timesTree :: Int -> Int
timesTree = (*3)

timesTree' :: Monad m => m Int -> m Int
timesTree' = liftM (*3) 

f2 :: Monad m => m a -> m b -> m (a, b)
-- f2 ma mb = do
--     a <- ma
--     b <- mb
--     return (a , b)
f2 = liftM2 (,)

f2' :: a -> b -> (a , b)
f2' = (,)

f3 :: Monad m => m (m a) -> m a
f3 = join

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 mf ma = do
    a <- ma
    f <- mf
    return (f a)

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
f8 = undefined

f9 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f9 = undefined
