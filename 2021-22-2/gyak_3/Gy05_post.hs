{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy05 where
import Control.Monad

-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas String-et
-- readLn   :: IO _                  -- beolvas bármit, ami beolvasható
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b

printNTimes :: Int -> String -> IO ()
printNTimes 0 s = return ()
printNTimes n s = do
  putStrLn s
  printNTimes (n - 1) s


-- Írj egy függvényt, ami beolvas egy sort, majd kiírja a sorban található
-- 'a' betűk számát.
io1 :: IO ()
-- io1 = do
--   l <- getLine
--   -- Lokális definíció `do` blokkon belül
--   -- let n = length (filter (=='a') l)
--   -- print n
--   print $ length (filter (=='a') l)

-- Függvénykopozícióval:
io1 = getLine >>= print . length . filter (=='a')


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = do
  -- line <- getLine
  -- putStrLn "-----"
  -- let len = length line
  -- printNTimes len line
  line <- getLine
  replicateM_ (length line) (putStrLn line)


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert.
io3 :: IO ()
io3 = do
  l <- getLine
  if 'x' `elem` l then do
    putStrLn "I found an 'x'!"
    return ()
  else
    io3

countLowerCase :: String -> Int
countLowerCase s = length (filter (`elem` ['a'..'z']) s)

-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát. A Ctrl-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = do
  l <- getLine
  let lc = countLowerCase l
  -- putStrLn (show lc)
  print lc
  io4

--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.

f1 :: Monad m => (a -> b) -> m a -> m b
-- f1 f ma = do
--   a <- ma
--   return (f a)
f1 = fmap

timesThree :: Monad m => m Int -> m Int
-- timesThree mi = do
--   i <- mi
--   return (3 * i)
timesThree = liftM (3*) -- fmap (<$>)

f2 :: Monad m => m a -> m b -> m (a, b)
-- f2 ma mb = do
--   a <- ma
--   b <- mb
--   return (a, b)
f2 = liftM2 (,)

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
