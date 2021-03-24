{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}

import Control.Applicative
import Control.Monad


{- State -}

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return a = State (\s -> (a, s))

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= g = State (\s -> case (f s) of (a, s') -> runState (g a) s')

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
push a = modify (a:)

pop :: Stack a a
pop = do
  s <- get
  case s of
   [] -> error "cannot pop the empty stack"
   (top:bottom) -> do
     put bottom
     return top

top :: Stack a a
top = head <$> get


{- Tasks -}

-- Define a new stack operation that checks if a stack is empty!
isEmpty :: Stack a Bool
isEmpty = undefined

-- Define a new stack operation that counts the number of stored elements!
depth :: Stack a Int
depth = undefined

-- Write a function that maps a function over a stack of elements, without
-- pattern matching, using only the stack operations!
mapStack :: (a -> a) -> Stack a ()
mapStack = undefined

-- execStack mapStackTest == [16, 14, 10]
mapStackTest :: Stack Int ()
mapStackTest = do
  push 5
  push 7
  push 8
  mapStack (*2)

-- Swap the elements of a pair!
pairSwap :: State (a, a) ()
pairSwap = undefined

-- execState pairSwap (3, 7) == (7, 3)

-- Execute an action in a local scope, returning to the original state
-- after it's done!
locally :: State s a -> State s a
locally = undefined

-- evalState locallyTest 5 == (10, 15)
locallyTest :: State Int (Int, Int)
locallyTest = do
  modify (*3)
  y <- locally (
    do
      modify (+2)
      x <- get
      pure (x - 7)
    )
  z <- get
  pure (y,z)


{- Tree duplication elimination -}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Functor, Foldable)

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


instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse = undefined


removeDuplicates :: (Tree Char, [Char]) -> (Tree Char, [Char])
removeDuplicates ((Leaf e), prev)   =
  if elem e prev
     then (Leaf ' ', prev)
     else (Leaf e, (e:prev))
removeDuplicates ((Node l r), prev) =
  let
    (leftResult, prev') = removeDuplicates (l, prev)
    (rightResult, prev'') = removeDuplicates (r, prev')
  in
    (Node leftResult rightResult, prev'')

removeDuplicates' :: Tree Char -> Tree Char
removeDuplicates' t = evalState (go t) [] where
  go :: Tree Char -> State [Char] (Tree Char)
  go (Leaf e)   = do
    prev <- get
    if elem e prev
       then pure (Leaf ' ')
       else do
         put (e:prev)
         pure (Leaf e)
  go (Node l r) = do
    cleanL <- go l
    cleanR <- go r
    pure (Node cleanL cleanR)

removeDuplicates'' :: Tree Char -> Tree Char
removeDuplicates'' = undefined


{- List monad -}
-- http://learnyouahaskell.com/a-fistful-of-monads

type Position = (Char, Int)

{-
    -----------------
  8 | | | | | | | | |
    -----------------
  7 | | | | | | | | |
    -----------------
  6 | | | | | | | | |
    -----------------
  5 | | | | | | | | |
    -----------------
  4 | | | | | | | | |
    -----------------
  3 | | | | | | | | |
    -----------------
  2 | | | | | | | | |
    -----------------
  1 | |x| | | | | | |
    -----------------
     a b c d e f g h
-}


-- Transpose a character in the alphabet
-- Examples:
--   + transposeChar   3  'a' == 'd'
--   + transposeChar (-2) 'h' == 'f'
transposeChar :: Int -> Char -> Char
transposeChar = undefined

-- Check if a position is valid on the board!
isValidOnBoard :: Position -> Bool
isValidOnBoard = undefined

-- List all the possible moves for a knight!
possibleKnightMoves :: [(Int, Int)]
possibleKnightMoves = undefined

-- List all the legal moves for a knight from a certain position!
knightMove :: Position -> [Position]
knightMove = undefined

-- List all the positions a knight can reach from B1 in two moves!
positionsInTwoMovesFromB1 :: [Position]
positionsInTwoMovesFromB1 = undefined
