{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}
{-# language DeriveFunctor #-}

import Control.Monad


filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = undefined

foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM' = undefined

whileM :: Monad m => m Bool -> m a -> m [a]
whileM = undefined

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ = undefined


f1 :: Monad m => (a -> b) -> m a -> m b
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined


{- State -}

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


{- Stack -}

type Stack a b = State [a] b

runStack :: Stack a b -> (b, [a])
runStack s = runState s []

evalStack :: Stack a b -> b
evalStack s = evalState s []

execStack :: Stack a b -> [a]
execStack s = execState s []


push :: a -> Stack a ()
push = undefined

pop :: Stack a a
pop = undefined

top :: Stack a a
top = undefined


stackTest :: Stack Char Char
stackTest = do
  push 'a'
  push 'b'
  push 'c'
  pop
  push 'd'
  pop

-- runStack stackTest == ('d', "ba")


{- RPN calculator -}
-- https://mathworld.wolfram.com/ReversePolishNotation.html

-- Adds the top two numbers
add :: Stack Int ()
add = undefined

-- Substracts the top number from the second from top number
substract :: Stack Int ()
substract = undefined

-- Multiplies the top two numbers
multiply :: Stack Int ()
multiply = undefined

-- Raises the second from top number to the power of the top number
power :: Stack Int ()
power = undefined

-- Swaps the top two numbers
swap :: Stack Int ()
swap = undefined

-- Push multiple elements at once
pushMultiple :: [a] -> Stack a ()
pushMultiple = undefined

-- (3 + 4)
-- evalStack testRPN1 == 7
testRPN1 :: Stack Int Int
testRPN1 = do
  push 3
  push 4
  add
  pop

-- (3 + 2 - 4 * 5)
-- evalStack testRPN2 == -15
testRPN2 :: Stack Int Int
testRPN2 = do
  pushMultiple [3, 2, 4, 5]
  multiply
  substract
  add
  top

-- (2 ^ (6 - 3))
-- evalStack testRPN3 == 8
testRPN3 :: Stack Int Int
testRPN3 = do
  push 6
  push 3
  substract
  push 2
  swap
  power
  top


{- Tree duplication elimination -}

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Node l r) = "[" ++ (show l) ++ " + " ++ (show r) ++ "]"

t :: Tree Char
t = Node
      (Node
         (Node (Leaf 'a') (Leaf 's'))
         (Node (Leaf 'd') (Leaf 'd')))
      (Node (Leaf 's') (Leaf 'f'))

t' :: Tree Char
t' = Node (Node (Leaf 'q') (Leaf 'q')) (Node (Leaf 'w') (Leaf 'q'))

t'' :: Tree Char
t'' = Node
       (Leaf 'a')
       (Node
          (Node (Leaf 'b') (Leaf 'a'))
          (Node (Leaf 'c') (Leaf 'a')))


removeDuplicates :: Tree Char -> Tree Char
removeDuplicates = undefined

removeDuplicates' :: Tree Char -> Tree Char
removeDuplicates' = undefined


{- Extra tasks -}

-- Execute an action in a local scope, returning to the original state
-- after it's done.
locally :: State s a -> State s a
locally = undefined

locallyTest :: State Int Int
locallyTest = do
  modify (*3)
  locally (
    do
      x <- get
      put (x + 2)
    )
  y <- get
  pure (y - 5)

-- runState locallyTest 5 == (10, 15)


-- Swaps the elements of a pair
pairSwap :: State (a, a) ()
pairSwap = undefined

-- execState pairSwap (3, 7) == (7, 3)
