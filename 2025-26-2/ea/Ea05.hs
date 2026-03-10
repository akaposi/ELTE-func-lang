module Ea05 where

import Control.Monad.State

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap f [] = []
  fmap f (x:xs) = f x : f x : fmap f xs -- wrong

Functor laws:
  fmap id x == x
  fmap f (fmap g x) == fmap (f . g) x

  optimizations

-- Monads ~ effects
class Functor m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)

instance Functor (State s)

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= g = State $ \s ->
    let (a, s') = runState m s
    in runState (g a) s'

Monad laws:
  -- left unit:
  (return x >>= f) == f x
  -- right unit:
  (m >>= \x -> return x) == m
  -- associativity:
  ((m >>= f) >>= g) == (m >>= \x -> (f x >>= g))

Monad laws using do-notation:
  (do y <- return x; f y) == f x
  (do x <- m; return x) == m
  (do y <- (do x <- m; f x); g y) == (do x <- m; y <- f x; g y)
-}

{-
get :: State s s
put :: s -> State s ()
-}

-- cumsum [1, 2, 3, 4] == [1, 3, 6, 10]
cumsum :: [Int] -> [Int]
cumsum xs = fst $ runState (helper xs) 0
  where
    helper :: [Int] -> State Int [Int]
    helper [] = return []
    helper (x:xs) = do
      oldSum <- get
      let newSum = oldSum + x
      put newSum
      xs' <- helper xs
      return $ newSum : xs'

-- askForData ["name", "email", "occupation"]
askForData :: [String] -> IO [String]
askForData [] = return []
askForData (x:xs) = do
  putStrLn $ "What's your " ++ x ++ "?"
  answer <- getLine
  rest <- askForData xs
  return $ answer : rest

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
  x' <- f x
  xs' <- mapM' f xs
  return $ x' : xs'

cumsum' :: [Int] -> [Int]
cumsum' xs = fst $ runState (mapM' helper xs) 0
  where
    helper x = do
      oldSum <- get
      let newSum = oldSum + x
      put newSum
      return newSum

askForDataThenPrint :: [String] -> IO ()
askForDataThenPrint xs = do
  userData <- askForData xs
  let lines = zipWith (\x y -> "Your " ++ x ++ " is " ++ y) xs userData
  mapM' putStrLn lines
  return ()

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m
  | n <= 0 = return []
  | otherwise = do
    x <- m
    xs <- replicateM (n - 1) m
    return $ x : xs

{-
map :: (a -> b) -> [a] -> [b]
mapM :: (a -> m b) -> [a] -> m [b]

replicate :: Int -> a -> [a]
replicateM :: Int -> m a -> m [a]

filter :: (a -> Bool) -> [a] -> [a]
filterM :: (a -> m Bool) -> [a] -> m [a]
-}

while :: Monad m => m Bool -> m () -> m ()
while cond body = do
  b <- cond
  if b
    then do
      body  -- _ <- body
      while cond body
    else return ()



-- List Monad

{-
instance Monad [] where
  return :: a -> [a]
  return x = [x]

  (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concat (map f xs)

  xs >>= f = concatMap f xs

  [] >>= _ = []
  (x:xs) >>= f = f x ++ (xs >>= f)

  xs >>= f = [y | x <- xs, y <- f x]
-}

type NonDet = []

data Flip = Heads | Tails
  deriving (Show, Eq)

flipCoin :: NonDet Flip
flipCoin = [Heads, Tails]

flipTwice :: NonDet (Flip, Flip)
flipTwice = do
  x <- flipCoin
  y <- flipCoin
  return (x, y)

flipFiveTimes :: NonDet [Flip]
flipFiveTimes = replicateM 5 flipCoin

impossible :: NonDet a
impossible = []

-- get all five flips where exactly two flips are heads
example :: NonDet [Flip]
example = do
  flips <- replicateM 5 flipCoin
  if length (filter (== Heads) flips) == 2
    then return ()
    else impossible
  return flips

-- List Monad = List comprehension

flipTwice' :: NonDet (Flip, Flip)
flipTwice' = [(x, y) | x <- flipCoin, y <- flipCoin]


{-
-- get all the end states of tic-tac-toe

guard :: Bool -> NonDet ()
guard b = if b then pure () else impossible

data XO = X | O
  deriving (Show, Eq)

type Board = [[Maybe XO]]

empty :: Board
empty = replicate 3 (replicate 3 Nothing)

xMove :: Board -> NonDet Board
xMove = do
  row <- [0..2]
  col <- [0..2]
  guard $ _ -- check the (row, col) is free
  _ -- update board

oMove :: Board -> NonDet Board
oMove = _

checkWin :: Board -> Bool
checkWin = _ -- check X or O has won

endStates :: NonDet Board
endStates = helper empty
  where
    helperX board
      | checkWin board = return board
      | checkTie board = return board
      | otherwise = do
        board' <- xMove
        helperO board'
    helperY board
      | checkWin board = return board
      | checkTie board = return board
      | otherwise = do
        board' <- oMove
        helperX board'
-}
