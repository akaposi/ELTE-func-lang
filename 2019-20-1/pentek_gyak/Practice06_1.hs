{-# LANGUAGE InstanceSigs #-}
module Practice06 where

import Control.Monad 

-- :k State --> * -> * -> *
newtype State s a = State (s -> (a,s))

-- If we ran the input and output computations,
-- their final states would be the same.
-- Only changes the value inside
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b 
  fmap f (State g) = State $ \s -> let (x,   s') = g s in 
                                       (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure = return
  
  (<*>) :: State s (a -> b) -> State s a -> State s b 
  (<*>) = ap -- in Control.Monad

-- * -> *
instance Monad (State s) where 
  -- Creates a computation which returns a constant
  return :: a -> State s a 
  return x = State $ \s -> (x,s) 

  -- Chains two computations together, 
  -- where the the latter one can depend on the result
  -- of the first one. 
  (>>=) :: State s a -> (a -> State s b) -> State s b 
  (>>=) (State f) k = State $ \s -> let (a,s')  = f s in
                                    let State g = k a in 
                                        g s'
                                    -- runState (k a) s'

-- Stateful computation which returns the current state.
get :: State s s 
get = State $ \s -> (s,s) 

-- Stateful computation which overrides the current state.
put :: s -> State s () 
put s = State $ \_ -> ((),s)

-- Given a stateful computation and a starting state 
-- runs the computation with the starting state.
-- The result is a pair consisting of the return value
-- of the computation and the final state.
runState :: State s a -> s -> (a, s)
runState (State f) s = f s 

-- Similar to runState, but only returns the return value
-- of the computation.
evalState :: State s a -> s -> a
evalState m = fst . runState m 

-- Similar to runState, but only returns the final state
-- of the computation.
execState :: State s a -> s -> s 
execState m = snd . runState m 
  