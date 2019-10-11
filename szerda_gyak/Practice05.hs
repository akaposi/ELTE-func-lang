{-# LANGUAGE InstanceSigs #-}
module Practice05 where

import Control.Monad

newtype State s a = State (s -> (a,s))

instance Functor (State s) where 
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> (f (fst $ g s), g s)

instance Applicative (State s) where 
  pure  = return
  (<*>) = ap

instance Monad (State s) where 
  return :: a -> State s a 
  return = undefined 
  
  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) = undefined

runState :: s -> State s a -> (a,s)
runState s (State f) = f s 

evalState :: s -> State s a -> a 
evalState s = fst . runState s 

execState :: s -> State s a -> s 
execState s = snd . runState s

get :: State s s
get = undefined 

put :: s -> State s ()
put = undefined

data BinTree a
  = Nil 
  | Branch a (BinTree a) (BinTree a)
  deriving (Eq, Ord, Show)

numberNodes :: BinTree a -> BinTree (Int, a)
numberNodes = fst . numberNodes' 0  where 
  numberNodes' :: Int -> BinTree a -> (BinTree (Int, a), Int)
  numberNodes' n Nil = (Nil, n) 
  numberNodes' n (Branch x l r) = (Branch (n,x) l' r', n'') where 
    (l', n')  = numberNodes' (n+1) l
    (r', n'') = numberNodes' n'    r