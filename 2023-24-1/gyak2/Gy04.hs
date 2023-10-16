{-# LANGUAGE DeriveFunctor #-}

module Gy04 where

import Control.Monad

{-
Monád definíció
class Functor m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
-}

-- Konstans bind művelet
-- f >> g = f >>= \_ -> g

-- IO műveletek
-- két üzenet kiírása
printTwo :: IO ()
printTwo = putStrLn "Hello" >> putStrLn "World"

-- do notáció
printTwo' :: IO ()
printTwo' = do
  putStrLn "Hello"
  putStrLn "World"

-- Írd ki egy lista összes elemét
-- print :: Show a => a -> IO ()
-- print = putStrLn . show
printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x : xs) = print x >> printList xs

-- getLine művelet
readAndPrint :: IO ()
readAndPrint = getLine >>= putStrLn

readAndPrint' :: IO ()
readAndPrint' = do
  ln <- getLine
  putStrLn ln

-- Olvassunk be X sort és az összegüket adjuk vissza
readAndCC :: Int -> IO String
readAndCC n | n <= 0 = return ""
readAndCC n = do
  ln <- getLine
  lns <- readAndCC (n - 1)
  return (ln ++ lns)

-- State monád, állapotváltozást reprezentál
newtype State s a = State { runState :: s -> (s, a) } deriving Functor

instance Applicative (State s) where
  pure a = State $ \s -> (s,a)
  (<*>) = ap

instance Monad (State s) where
  (State sa) >>= f = State $ \s -> let (s', a) = sa s in runState (f a) s'


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const (s,())

modify :: (s -> s) -> State s ()
modify f = get >>= put . f
-- Primitív kombinátorok
-- Minden állapotváltozás felépíthető get, put és >>= műveletekkel

-- Adjunk hozzá a State-hez 1-et
add1 :: State Int ()
add1 = get >>= \x -> put (x + 1) -- vagy modify (+1)

-- Adjuk egy listában a számokat össze és űrítsük ki a listát
sumState :: Num a => State [a] a
sumState = do
  xs <- get
  case xs of
    [] -> return 0
    (x:xs) -> do
      put xs
      x' <- sumState
      return (x + x')
