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
empty' :: Stack a Bool
-- empty = do
--   stack <- get
--   if length stack == 0 then pure True else pure False
-- empty = ((==0) . length) <$> get
empty' = null <$> get

-- Define a new stack operation that counts the number of stored elements!
depth :: Stack a Int
depth = length <$> get

-- Write a function that maps a function over a stack of elements, without
-- pattern matching, using only the stack operations!
mapStack :: (a -> a) -> Stack a ()
mapStack f = do
  e <- empty'
  when (not e) (do
    c <- pop
    mapStack f
    push (f c)
    )

-- execStack mapStackTest == [16, 14, 10]
mapStackTest :: Stack Int ()
mapStackTest = do
  push 5
  push 7
  push 8
  mapStack (*2)

-- Swap the elements of a pair!
pairSwap :: State (a, a) ()
pairSwap = do
  -- pair <- get
  -- case pair of (a, b) -> put (b, a)
  -- put (snd pair, fst pair)
  (a, b) <- get
  put (b, a)

-- execState pairSwap (3, 7) == (7, 3)

-- Execute an action in a local scope, returning to the original state
-- after it's done!
locally :: State s a -> State s a
locally sa = do
  pre <- get
  a <- sa
  put pre
  pure a

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
  traverse f (Leaf e)   = Leaf <$> (f e)
  traverse f (Node l r) = Node <$> (traverse f l) <*> (traverse f r)
  -- traverse f (Node l r) = let
  --   left = traverse f l
  --   right = traverse f r
  --   in
  --     liftA2 Node left right


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
  go (Node l r) = Node <$> go l <*> go r
  -- go (Node l r) = do
  --   cleanL <- go l
  --   cleanR <- go r
  --   pure (Node cleanL cleanR)

removeDuplicates'' :: Tree Char -> Tree Char
removeDuplicates'' t = evalState (traverse go t) [] where
  go :: Char -> State [Char] Char
  go c = do
    prev <- get
    if elem c prev
       then pure ' '
       else do
         put (c:prev)
         pure c

-- t''' :: Tree Int
t''' :: Num a => Tree a
t''' = Node (Node (Leaf 3) (Leaf 7)) (Node (Leaf 19) (Leaf 2))

flipSigns :: Tree Int -> Tree Int
flipSigns t = evalState (traverse go t) False where
  go :: Int -> State Bool Int
  go c = do
    flip <- get
    put (not flip)
    if flip then (pure (-c)) else pure c

-- a = flipSigns t'''
