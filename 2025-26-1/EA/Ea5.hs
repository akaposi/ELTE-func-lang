module Ea5 where

import Control.Monad.State
import Data.List
import Debug.Trace

-- Maybe
-- State s a = s -> (a, s)

-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- cumsum [1, 2, 3, 4] = [1, 3, 6, 10]
cumsum :: [Int] -> [Int]
cumsum xs = fst (runState (go xs) 0)
  where
  go :: [Int] -> State Int [Int]
  go [] = return []
  go (x : xs) = do
    sumSoFar <- get
    let newSum = sumSoFar + x
    put newSum
    xs' <- go xs
    return (newSum : xs')

-- get :: State s s
-- put :: s -> State s ()
-- runState :: State s a -> s -> (a, s)

-- IO, print, putStrLn, readLine

printList :: [Int] -> IO ()
printList [] = return ()
printList (x:xs) = do
  print x
  printList xs

-- ["name", "address", "age"]
-- What's your name?
-- asd
-- What's your address?
-- ...
askForData :: [String] -> IO [String]
askForData [] = return []
askForData (x:xs) = do
  putStrLn $ "What's your " ++ x ++ "?"
  answer <- getLine
  answers <- askForData xs
  return (answer : answers)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
  y <- f x
  ys <- mapM' f xs
  return (y:ys)

cumsum' :: [Int] -> [Int]
cumsum' xs = fst (runState (go xs) 0)
  where
  go :: [Int] -> State Int [Int]
  go = mapM $ \x -> do
    sumSoFar <- get
    let newSum = sumSoFar + x
    put newSum
    return newSum

printList' :: [Int] -> IO ()
printList' xs = do
  mapM print xs
  return ()

askForData' :: [String] -> IO [String]
askForData' = mapM $ \x -> do
  putStrLn $ "What's your " ++ x ++ "?"
  answer <- getLine
  return answer

-- map :: (a -> b) -> [a] -> [b]
-- mapM :: (a -> m b) -> [a] -> m [b]

-- replicate :: Int -> a -> [a]
-- replicateM :: Int -> m a -> m [a]

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m
  | n <= 0 = return []
  | otherwise = do
    x <- m
    xs <- replicateM (n - 1) m
    return (x:xs)

-- IO is a functor
-- every monad is a functor

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f m = do
  a <- m
  return (f a)

-- filterM :: (a -> m Bool) -> [a] -> m [a]
-- foldlM :: (b -> a -> m b) -> b -> [a] -> m b

-- [] is a monad

type NonDet = []

data CoinFlip = Heads | Tails
  deriving (Show, Eq)

flipCoin :: NonDet CoinFlip
flipCoin = [Heads, Tails]

example :: NonDet (CoinFlip, CoinFlip)
example = do
  x <- flipCoin
  y <- flipCoin
  return (x, y)

impossible :: NonDet ()
impossible = []

-- flip 5 times then all the possibilities where exactly 2 is heads
example2 :: NonDet [CoinFlip]
example2 = do
  flips <- replicateM 5 flipCoin
  if length (filter (== Heads) flips) == 2
    then return ()
    else impossible
  return flips

return' :: a -> NonDet a
return' a = [a]

-- >>=
andThen :: NonDet a -> (a -> NonDet b) -> NonDet b
andThen [] f = []
andThen (a:as) f = f a ++ andThen as f

-- instance Monad []

-- list comprehension = list monad

example' :: [(CoinFlip, CoinFlip)]
example' = [(x, y) | x <- flipCoin, y <- flipCoin]

example2' :: [[CoinFlip]]
example2' =
  [
    [f1, f2, f3, f4, f5] |
    f1 <- flipCoin,
    f2 <- flipCoin,
    f3 <- flipCoin,
    f4 <- flipCoin,
    f5 <- flipCoin,
    length (filter (== Heads) [f1, f2, f3, f4, f5]) == 2
  ]

andThen' :: [a] -> (a -> [b]) -> [b]
andThen' xs f = concatMap f xs


-- solving sudoku

-- maybe :: b -> (a -> b) -> Maybe a -> b


type Sudoku = [[Maybe Int]]

validInRow :: Int -> Int -> [[Maybe Int]] -> Bool
validInRow d row sudoku =
  all (\column -> maybe True (/= d) (sudoku !! row !! column)) [0..8]

validInColumn :: Int -> Int -> [[Maybe Int]] -> Bool
validInColumn d column sudoku =
  all (\row -> maybe True (/= d) (sudoku !! row !! column)) [0..8]

whichCell :: Int -> Int
whichCell x
  | x < 3 = 0
  | x < 6 = 1
  | otherwise = 2

validInCell :: Int -> (Int, Int) -> [[Maybe Int]] -> Bool
validInCell d (row, column) sudoku =
  all (\cellRow ->
    all (\cellColumn ->
      maybe True (/= d) (sudoku !! (whichCell row * 3 + cellRow) !! (whichCell column * 3 + cellColumn))
    ) [0..2]
  ) [0..2]

valid :: Int -> (Int, Int) -> [[Maybe Int]] -> Bool
valid d (row, column) sudoku =
  validInRow d row sudoku &&
  validInColumn d column sudoku &&
  validInCell d (row, column) sudoku

guard :: Bool -> NonDet ()
guard b = if b then return () else []

solve :: Sudoku -> [Sudoku]
solve = go (0, 0)
  where
  go :: (Int, Int) -> Sudoku -> NonDet Sudoku
  go (i, j) sudoku
    | i == 9 = return sudoku
    | j == 9 = go (i + 1, 0) sudoku
    | otherwise = case sudoku !! i !! j of
        Just _ -> go (i, j + 1) sudoku
        Nothing -> do
          d <- [1 .. 9]
          guard $ valid d (i, j) sudoku
          let newSudoku =
                zipWith (\i' row ->
                  if i == i' then
                    zipWith (\j' oldD -> if j == j' then Just d else oldD) [0..] row
                  else row)
                  [0..] sudoku
          go (i, j + 1) newSudoku

-- slow for hard sudokus

exampleSudoku :: Sudoku
exampleSudoku =
  map (map (\x -> if x == 0 then Nothing else Just x))
  [[0,4,0,0,0,0,1,7,9]
  ,[0,0,2,0,0,8,0,5,4]
  ,[0,0,6,0,0,5,0,0,8]
  ,[0,8,0,0,7,0,9,1,0]
  ,[0,5,0,0,9,0,0,3,0]
  ,[0,1,9,0,6,0,0,4,0]
  ,[3,0,0,4,0,0,7,0,0]
  ,[5,7,0,1,0,0,2,0,0]
  ,[9,2,8,0,0,0,0,6,0]
  ]
