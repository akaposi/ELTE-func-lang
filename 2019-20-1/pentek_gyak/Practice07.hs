{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Practice07 where

import Control.Monad
import Control.Monad.State

newtype Writer w a = Writer (State w a)
  deriving (Functor, Applicative, Monad)

runWriter :: Monoid w => Writer w a -> (a,w)
runWriter (Writer s) = runState s mempty

tell :: Monoid w => w -> Writer w ()
tell x = Writer $ do 
  s <- get 
  put (s <> x)

newtype Writer2 w a = Writer2 { runWriter2 :: (a, w) }

instance Functor (Writer2 w) where 
  fmap :: (a -> b) -> Writer2 w a -> Writer2 w b
  fmap f (Writer2 (a, w)) = Writer2 (f a, w)

instance Monoid w => Applicative (Writer2 w) where 
  pure  = return 
  (<*>) = ap

instance Monoid w => Monad (Writer2 w) where 
  return :: a -> Writer2 w a 
  return x = Writer2 (x, mempty)

  (>>=) :: Writer2 w a -> (a -> Writer2 w b) -> Writer2 w b
  (>>=) (Writer2 (x, w)) k = Writer2 (x', w <> w') where 
    Writer2 (x', w') = k x 

tell2 :: Monoid w => w -> Writer2 w () 
tell2 x = Writer2 ((), x)

-- Writer2 is inefficient
example :: Writer2 [Int] ()
example = do 
  return ()
  return ()
  return ()

-- result = ((), mempty <> mempty <> mempty)

type CMap a       = [(a, Int)]
type CounterM a b = State (CMap a) b 

-- simple recursion
inc :: Eq a => a -> CMap a -> CMap a
inc x [] = [(x,1)]
inc x ((y,n):ys) 
  | x == y    = (y,n+1) : ys 
  | otherwise = (y,n)   : inc x ys

-- use DO, use inc, similar to pushM 
incM :: Eq a => a -> CounterM a () 
incM x = do 
  cmap <- get 
  let cmap' = inc x cmap
  put cmap'

-- use DO, use incM, similar pushAllM
countM :: Eq a => [a] -> CounterM a ()
countM [] = pure () 
countM (x:xs) = do 
  incM x
  countM xs 
  

-- use execState
execCounter :: CounterM a b -> CMap a 
execCounter m = execState m []

-- use countM, execCounter, lookup :: Eq a => a -> [(a,b)] -> Maybe b
-- execute the counterM computation
count :: Eq a => a -> [a] -> Int
count x xs = case lookup x occurences of 
  Nothing -> 0 
  Just n -> n
  where
    occurences = execCounter (countM xs)