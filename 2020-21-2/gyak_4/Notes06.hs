{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}

module Notes06 where

import Control.Monad

-- State monad

newtype State s a = State { runState :: s -> (a, s) } 
                  deriving(Functor)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

-- In imperative programming      
--    put expr    ~       state := expr
put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

-- Example: 
incr :: State Int ()
incr = modify (+1)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

-- Translation of imperative programs using the State monad.

-- forM :: Monad m => [a] -> (a -> m b) -> m [b]
-- forM_ :: Monad m => [a] -> (a -> m b) -> m () -- forM_ discards the results of forM

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond ma = do
  b <- cond 
  if b then (:) <$> ma <*> whileM cond ma 
       else pure []

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond ma = whileM cond ma >> pure ()

-- Example:
--   x := 1
--   for i from 1 to n
--     x := x + 1
ex :: Integer -> State Integer ()
ex n = do
  put 1                   -- x := 1
  forM_ [1..n] $ \_ -> do -- for i from 1 to n
    modify (\x -> x + 1)  -- x := x+1

runEx :: Integer -> Integer
runEx n = execState (ex n) 1

-- impFactorial should be a translation of the imperative program
--    x := 1
--    for i from 1 to n
--      x := x * i

impFactorial :: Integer -> State Integer ()
impFactorial n = undefined

runFactorial :: Integer -> Integer
runFactorial n = execState (impFactorial n) 1

-- impFibo should be a translation of the imperative program
--    (a, b) := (1, 1)
--    for i from 1 to n
--      (a, b) := (b, a+b)

impFibo :: Integer -> State (Integer, Integer) ()
impFibo n = undefined

runFibo :: Integer -> Integer
runFibo n = fst (execState (impFibo n) (1, 1))

-- impGcd should be a translation of the imperative program 
--   (a, b) are inputs
--   while b /= 0
--     (a, b) := (b, a `mod` b)

impGcd :: State (Integer, Integer) ()
impGcd = undefined

runGcd :: Integer -> Integer -> Integer
runGcd x y = fst $ execState impGcd (x, y)



-- Stack machine interpreter / Reverse Polish notation

-- Here the state is a list (or a stack) of integers.
-- Operations pop some inputs from the top of the stack 
--  and push their result on the stack.

type M a = State [Integer] a

push :: Integer -> M ()
push = undefined

pop :: M Integer
pop = undefined

plus :: M ()
plus = undefined

unaryOp :: (Integer -> Integer) -> M ()
unaryOp = undefined

negate' :: M ()
negate' = unaryOp negate

binaryOp :: (Integer -> Integer -> Integer) -> M ()
binaryOp = undefined

-- plus' = binaryOp (+)

-- Examples:
p1 :: M Integer
p1 = do
  push 10  -- [10]
  push 20  -- [20,10]
  plus     -- [30]
  pop   
-- evalState [] p1 == 30

p2 :: M Integer
p2 = do
  push 10  -- [10]
  push 20  -- [20,10]
  push 30  -- [30,20,10]
  plus     -- [50,10]
  push 1   -- [1,50,10]
  negate'  -- [-1,50,10]
  plus     -- [49,10]
  plus     -- [59]
  pop
-- evalState [] p2 == 59

-- Bonus:
newtype StateMaybe s a = StateMaybe { runStateMaybe :: s -> Maybe (s, a) }
                       deriving (Functor)
-- StateMaybe s should generalize both the state monad and the Maybe monad.

-- Define Applicative and Monad instances for StateMaybe.

-- Define an operation `pop' :: StateMaybe [Integer] Integer` 
--  that does not throw an exception when the stack is empty.
