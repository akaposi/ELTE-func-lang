module Gy06 where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

labelBwSt :: Num i => [a] -> State i [(a, i)]
labelBwSt [] = return []
labelBwSt (x : xs) = do
  xs' <- labelBwSt xs
  i <- get
  put (i + 1)
  return ((x, i) : xs')

labelBw :: Num i => [a] -> [(a,i)]
labelBw xs = evalState (labelBwSt xs) 0 -- evalState s a = snd (runState s a)

f :: Writer [String] Int
f = do
  writer (1, ["almafa"])
  writer (2, ["balmafa", "calmafa"])
  return 12

f' :: Writer [String] (Int, [String])
f' = listen $ do
  tell ["almafa"]
  tell ["balmafa", "calmafa"]
  return 12

removeAlmas :: Writer [String] Int
removeAlmas = pass $ do
  let x = 1
  tell ["alma"]
  let y = x + 1
  tell ["balma"]
  tell ["alma"]
  return (y * y, \w -> filter (/= "alma") w)

silence :: Monoid w => Writer w a -> Writer w a
silence w = pass (fmap (\a -> (a, mempty)) w)

-- get :: State s s
-- put :: s -> State s ()


--               v home directory
data Env = MkEnv String Bool deriving Show
--                       ^ admin / root

isAdmin :: Reader Env Bool
isAdmin = reader (\(MkEnv _ b) -> b)

giveHomeDirectory :: Reader Env String
giveHomeDirectory = reader (\(MkEnv h _) -> h)

h :: Reader Env Env
h = do
  a <- local (\(MkEnv _ b) -> MkEnv "/home/gergoke" b) ask
  return a
