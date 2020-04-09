
-- Monád folyt: State, lista, stb
--------------------------------------------------------------------------------

import Control.Monad  -- ap

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s -> case g s of (a, s) -> (f a, s)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> (runState (g a)) s'
