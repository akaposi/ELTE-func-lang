{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}
{-# language DeriveFunctor #-}

import Control.Monad

-- multByEightAndAddThree :: Int -> Int
-- multByEightAndAddThree = (+3) . (*8)
-- (f ∘ g)(x) = f(g(x))

-- addAndMultByEight :: Int -> Int -> Int
-- (+) :: Int -> (Int -> Int)
-- addAndMultByEight x y = (x + y) * 8
-- ((*8) .) :: (Int -> Int) -> (Int -> Int)
-- addAndMultByEight = ((*8) .) . (+)

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = pure []
filterM' f (a:as) = do
  b <- f a
  rest <- filterM' f as
  pure (if b then a:rest else rest)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = pure []
mapM' f (a:as) = do
  fa <- f a
  rest <- mapM' f as
  pure (fa:rest)

smallAdd :: Int -> Int -> Maybe Int
smallAdd x y = if (x + y) < 1000 then Just (x + y) else Nothing

foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM' f b ta =
  foldr
    (\a mb -> do
      b' <- mb
      f a b'
      )
    (pure b)
    ta

-- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html
whileM :: Monad m => m Bool -> m a -> m [a]
whileM mb ma = do
  b <- mb
  if b
    then do
      a <- ma
      as <- whileM mb ma
      pure (a:as)
    else do
      pure []

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ mb ma = whileM mb ma >> pure ()

f1 :: Monad m => (a -> b) -> (m a -> m b)
f1 = liftM
-- f1 = fmap
-- f1 = (<$>)

-- (,) ::          a ->   b ->   (a, b)
f2 :: Monad m => m a -> m b -> m (a, b)
f2 = liftM2 (,)

f3 :: Monad m => m (m a) -> m a
f3 = join

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = (<*>)

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = (=<<)

f6 :: Monad m => (a -> b -> c) -> (m a -> m b -> m c)
f6 = liftM2

f7 :: Monad m => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)
f7 = liftM3

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = (>=>)


getMaxLength :: [String] -> String
getMaxLength []   = ""
getMaxLength (a:as) =
  let
    rest = getMaxLength as
  in
    if length rest > length a then rest else a

io3 :: IO ()
io3 = do
  n <- readLn
  lines <- replicateM n getLine
  print (getMaxLength lines)


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


{- Record syntax example: -}

-- data Person = MkPerson Int String Bool deriving (Show)
--
-- getAge :: Person -> Int
-- getAge (MkPerson age _ _) = age

data Person = MkPerson {age :: Int, name :: String, isGirl :: Bool} deriving (Show)
isti = MkPerson 24 "Isti" False
-- age Isti == 24
-- isGirl isti == False


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

stateTest :: State Int Bool
stateTest = do
  x <- get
  put (3 * x)
  modify (+2)
  y <- get
  return (even y)

-- runState stateTest 5 == (False, 17)


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
-- push a = do
--   s <- get
--   put (a:s)

pop :: Stack a a
pop = do
  s <- get
  case s of
   [] -> error "cannot pop the empty stack"
   (top:bottom) -> do
     put bottom
     return top

top :: Stack a a
top = do
  s <- get
  return (head s)


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
add = do
  x <- pop
  y <- pop
  push (x + y)

-- Substracts the top number from the second from top number
substract :: Stack Int ()
substract = do
  x <- pop
  y <- pop
  push (y - x)

-- Multiplies the top two numbers
multiply :: Stack Int ()
multiply = do
  x <- pop
  y <- pop
  push (x * y)

-- Raises the second from top number to the power of the top number
power :: Stack Int ()
power = do
  x <- pop
  y <- pop
  push (y ^ x)

-- Swaps the top two numbers
swap :: Stack Int ()
swap = do
  x <- pop
  y <- pop
  push x
  push y

-- Push multiple elements at once
pushMultiple :: [a] -> Stack a ()
pushMultiple [] = pure ()
pushMultiple (a:as) = do
  push a
  pushMultiple as

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

-- whileM example:

testRPN4 :: Stack Int Int
testRPN4 = do
  push 3
  whileM ((< 30) <$> top)
    -- (top >>= (return . (< 30)))
    -- (do {x <- top; return (x < 30)})
    (do
      x <- top
      push (x + 5)
    )
  top


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
