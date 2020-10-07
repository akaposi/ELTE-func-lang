{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes05 where

import Control.Monad (ap, forM, forM_)

--------------------------------------------------------------------------------

-- Also in Control.Monad.State
newtype State s a = State { runState :: s -> (s, a) }
                  deriving(Functor)

execState :: State s a -> s -> s
execState (State f) s = fst (f s)

evalState :: State s a -> s -> a
evalState (State f) s = snd (f s)

put :: s -> State s ()
put s = State (\_ -> (s, ()))

get :: State s s
get = State (\s -> (s, s))

instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad (State s) where
  return x = State (\s -> (s, x))
  State f >>= g = State (\s -> let (s', a) = f s in runState (g a) s')

--------------------------------------------------------------------------------


-- Define modify using   State (\s -> ...)
modify :: (s -> s) -> State s ()
modify f = State undefined

-- Define modify' using put and get
modify' :: (s -> s) -> State s ()
modify' f = do
  undefined

-- "Translations" of imperative programs using the State monad

-- Example:
--   x = 1
--   for i from 1 to n
--     x = x + 1
ex :: Integer -> State Integer ()
ex n = do
  put 1                   -- x = 1
  forM_ [1..n] $ \i -> do -- for i from 1 to n
    x <- get 
    put (x+1)             --   x = x+1

-- impFactorial should be a translation of the imperative program
--    x = 1
--    for i from 1 to n
--      x = x * i

impFactorial :: Integer -> State Integer ()
impFactorial n = undefined

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    a = 1
--    b = 1
--    for i from 1 to n
--      (a, b) = (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = undefined

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))

-- impGcd should be a translation of the imperative program 
--   (a, b) are inputs
--   while b /= 0
--     (a, b) = (b, a `mod` b)
--
--   (you may want to define whileM first)

impGcd :: State (Integer, Integer) ()
impGcd = undefined

runGcd :: Integer -> Integer -> Integer
runGcd x y = fst $ execState impGcd (x, y)

-- 

whileM :: Monad m => m Bool -> m a -> m ()
whileM = undefined

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a] 
takeWhileM = undefined

--