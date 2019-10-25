{-# LANGUAGE InstanceSigs #-}
module Practice06 where

import Control.Monad 

-- :k State --> * -> * -> *
newtype State s a = State (s -> (a,s))

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b 
  fmap = undefined

instance Applicative (State s) where
  pure :: a -> State s a
  pure = return
  
  (<*>) :: State s (a -> b) -> State s a -> State s b 
  (<*>) = ap -- in Control.Monad

-- * -> *
instance Monad (State s) where 
  return :: a -> State s a 
  return = undefined 

  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) = undefined 

get :: State s s 
get = undefined 

put :: State s () 
put = undefined 

runState :: State s a -> s -> (a, s)
runState = undefined 

evalState :: State s a -> s -> a
evalState = undefined 

execState :: State s a -> s -> s 
execState = undefined
  