{-# LANGUAGE InstanceSigs #-}
module Practice06 where

import Control.Monad 

-- :k State --> * -> * -> *
newtype State s a = State (s -> (a,s))

-- The output state is the same as the input state
-- Only changes the value inside
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
  -- Creates a computation which returns a cosntant
  return :: a -> State s a 
  return = undefined 

  -- Chains two computations together, 
  -- where the the latter one can depend on the result
  -- of the first one. 
  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) = undefined 

-- Stateful computation which returns the current state.
get :: State s s 
get = undefined 

-- Stateful computation which overrides the current state.
put :: s -> State s () 
put = undefined 

-- Given a stateful computation and a starting state 
-- runs the computation with the starting state.
-- The result is a pair consisting of the return value
-- of the computation and the final state.
runState :: State s a -> s -> (a, s)
runState = undefined 

-- Similar to runState, but only returns the return value
-- of the computation.
evalState :: State s a -> s -> a
evalState = undefined 

-- Similar to runState, but only returns the final state
-- of the computation.
execState :: State s a -> s -> s 
execState = undefined
  