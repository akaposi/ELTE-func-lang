
{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad

--------------------------------------------------------------------------------
-- Következő feladat: rekurzív IO feladat megint

--------------------------------------------------------------------------------

-- ismételten: beolvas egy sort, kinyomtatja a hosszát
-- getLine :: IO String
-- print   :: Show a => a -> IO ()
-- print   :: Int -> IO ()

f :: IO ()
f = do
  l <- getLine      -- getLine :: IO String
                    -- l :: String
  print (length l)
  f

f' :: IO ()
f' = forever $ do
  l <- getLine
  print (length l)

{-
loop :: a
loop = loop

forever :: Monad m => m a -> m b
forever ma = do
  ma
  forever ma
-}

-- Monád segéd függvények/operátorok

-- (>>) :: Monad m => m a -> m b -> m b       -- "kontans bind"
-- (>>) ma mb = ma >>= \_ -> mb

-- (egymás után végrehajtás)

-- alternatív:
-- (>>) ma mb = do
--   ma
--   mb

{-
forever :: Monad m => m a -> m b
forever ma = ma >> forever ma
-}

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
-- (=<<) = flip (>>=)


f'' :: IO ()
f'' = forever $
  print . length =<< getLine
  -- length :: [a] -> Int
  -- print  :: Int -> IO ()
  -- print . length :: [a] -> IO ()
  -- print . length =<< getLine :: IO ()

  -- imperatív nyelvben: print(length(getLine()));


--------------------------------------------------------------------------------
-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = goio3 []

-- vegyünk fel egy függvényt, aminek a paraméter a beolvasott sorok
-- listája:

goio3 :: [String] -> IO ()
goio3 lines = do
  l <- getLine
  if elem 'x' l then
    printAllLines (l:lines)
  else
    goio3 (l:lines)

-- hátulról előre nyomtassuk ki az összes sort
printAllLines :: [String] -> IO ()
printAllLines [] = return ()
printAllLines (l:lines) = do
  printAllLines lines
  putStrLn l

goio3' :: [String] -> IO ()
goio3' lines = do
  l <- getLine
  if elem 'x' l then
    mapM_ putStrLn (l:lines)  -- balról jobbra bejárja a listát
  else
    goio3 (l:lines)

-- listákra standard függvények 3 másolatban vannak:

-- tiszta, monádikus, monádikus + visszatérési érték ()

-- map    ::            (a ->   b) -> [a] ->   [b]
-- mapM   :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_  :: Monad m => (a -> m b) -> [a] -> m ()

-- filter   ::            (a ->   Bool) -> [a] ->   [a]
-- filterM  :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM_  (nincs!)

-- zipWith
-- zipWithM
-- zipWithM_

-- forM = flip mapM
-- forM_ = flip mapM_

-- forM_: for ciklust lehet vele írni

foo :: IO ()
foo = forM_ [0..100] $ \i -> print (i + 20)

foo2 :: IO ()
foo2 = forM_ [0..10] $ \i ->
         forM_ [0..10] $ \j ->
           print (i + j)

-- egymás után hajtsuk végre az összes műveletet egy listában
-- adjuk vissza a visszatérési értékek listáját
-- sequence :: Monad m => [m a] -> m [a]

-- foo3 = sequence [putStrLn "foo", putStrLn "bar]
--      = putStrLn "foo" >> putStrLn "bar

-- foo2 = sequence [print (i + j) | i <- [0..10], j <- [0..10]]


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = forever $ do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l

--------------------------------------------------------------------------------
-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.


f1 :: Monad m => (a -> b) -> m a -> m b
f1 f ma = do
  a <- ma
  return (f a)

f2 :: Monad m => m a -> m b -> m (a, b)
f2 ma mb = do
  a <- ma
  b <- mb
  return (a, b)

f3 :: Monad m => m (m a) -> m a
f3 mma = do
  ma <- mma
  ma

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 f ma = do
  a <- ma
  f a

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 f ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return (f a b c)

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 f g a = do
  b <- f a
  g b
