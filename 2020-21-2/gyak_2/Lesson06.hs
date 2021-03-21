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


-- Írj egy függvényt, ami addig olvas be ismételten sorokat stdin-ről,
-- amíg a sor nem tartalmaz 'x' karaktert.
-- Ha a sorban 'x' van, akkor a program nyomtassa ki az összes
-- eddig beolvasott sort és térjen vissza.
io2 :: IO ()
io2 = undefined


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


-- Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Node l r) = "[" ++ (show l) ++ " + " ++ (show r) ++ "]"

t :: Tree Int
t = Node
      (Node
         (Node (Leaf 10) (Leaf 7))
         (Node (Leaf 3) (Leaf 4)))
      (Node (Leaf 7) (Leaf 3))

t' :: Tree Int
t' = Node (Node (Leaf 7) (Leaf 10)) (Node (Leaf 10) (Leaf 10))

t'' :: Tree Char
t'' = Node
       (Leaf 'a')
       (Node
          (Node (Leaf 'b') (Leaf 'a'))
          (Node (Leaf 'c') (Leaf 'a')))


removeDuplicates :: Tree Int -> Tree Int
removeDuplicates = undefined

removeDuplicates' :: Tree Int -> Tree Int
removeDuplicates' = undefined

-- Stack

push :: Int -> State [Int] ()
push n = modify (\ns -> n:ns)

pop :: State [Int] (Maybe Int)
pop = do
  ns <- get
  case ns of
    []   -> pure Nothing
    n:ns -> do
      put ns
      pure (Just n)

-- RPN calculator
-- https://mathworld.wolfram.com/ReversePolishNotation.html

-- Adds the top two numbers
add :: State [Int] ()
add = undefined

substract :: State [Int] ()
substract = undefined

multiply :: State [Int] ()
multiply = undefined

power :: State [Int] ()
power = undefined

-- Swaps the top two numbers
swap :: State [Int] ()
swap = undefined

test :: State [Int] (Maybe Int)
test = do
  push 3
  push 4
  add
  pop

calculate :: State [Int] (Maybe Int) -> (Maybe Int)
calculate c = evalState c []

-- calculate test == 7

locally :: State s a -> State s a
locally = undefined

-- Swaps the elements of a pair
swap' :: State (a, a) ()
swap' = undefined
