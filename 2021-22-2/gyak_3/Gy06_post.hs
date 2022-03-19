{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy06 where

import Control.Monad

getYesNo :: IO Bool
getYesNo = do
  line <- getLine
  case line of
    "Y" -> return True
    "N" -> return False
    _   -> do
      putStrLn "Please enter 'Y' or 'N'!"
      getYesNo

  -- if line == "Y" then return True else
  --   if line == "N" then return False else do
  --   putStrLn "Please enter 'Y' or 'N'!"
  --   getYesNo

getSafeNumber :: IO Int
getSafeNumber = do
  line <- getLine
  if
    all (`elem` ['0'..'9']) line && length line /= 0
  then
    return (read line)
  else do
    putStrLn "Please enter a valid number!"
    getSafeNumber

-- What's your name?
-- > István
-- Hello, István!
-- When were you born?
-- > 1997
-- Did you already have your birthday this year? (Y/N)
-- > Y
-- You are 25 years old.
io5 :: IO ()
io5 = do
  putStrLn "What's you name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"
  putStrLn "When were you born?"
  -- year <- getLine
  -- putStrLn $ "Teszt:" ++ show (2022 - read year)
  -- year <- readLn
  year <- getSafeNumber
  putStrLn "Did you already have your birthday this year? (Y/N)"
  bday <- getYesNo
  let age = if bday then 2022 - year else 2022 - year - 1
  putStrLn $ "You are " ++ show age ++ " old."

--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.

f1 :: Monad m => (a -> b) -> m a -> m b
-- f1 f ma = do
--   a <- ma
--   return (f a)
-- f1 = fmap
f1 = (<$>)

f2 :: Monad m => m a -> m b -> m (a, b)
-- f2 ma mb = do
--   a <- ma
--   b <- mb
--   return (a, b)
f2 = liftM2 (,)

f3 :: Monad m => m (m a) -> m a
-- f3 mma = do
--   ma <- mma
--   -- a <- ma
--   -- return a
--   ma
f3 = join

bind' :: Monad m => m a -> (a -> m b) -> m b
bind' = undefined

-- fmap :: Functor f =>   (a -> b) -> f a -> f b
f4      :: Monad m   => m (a -> b) -> m a -> m b
f4 = (<*>)
-- f4 mab ma = do
--   ab <- mab
--   a <- ma
--   return (ab a)

f5 :: Monad m => (a -> m b) -> m a -> m b
-- f5 = flip (>>=)
f5 = (=<<)

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = liftM2

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = liftM3

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = (>=>)


-- State monád definíció
--------------------------------------------------------------------------------

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
modify f = do { st <- get ; put (f st) }

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--

stateExample :: State Int Bool
stateExample = do
  put 10
  let a = 20
  c <- get
  put (2 + c)
  d <- get
  return (even d)

triple :: State Int ()
triple = modify (*3)
-- triple = do
--   st <- get
--   put (3 * st)

addTwo :: State Int ()
addTwo = modify (+2)
-- addTwo = do
--   st <- get
--   put (2 + st)

timesNinePlusSix :: State Int ()
timesNinePlusSix = do
  triple
  addTwo
  triple

-- Should be ((5 * 3) + 2) * 3 = 5 * 9 + 6 = 51
test :: Int
test = execState timesNinePlusSix 5

--


{- Stack -}

-- Add an element to the top of the stack by extending the state list!
push :: a -> State [a] ()
push a = modify (a:)
-- push a = do
--   st <- get
--   put (a:st)

pushTest :: State [Int] ()
pushTest = do
  push 1
  push 2
  push 3

-- Examples:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Define a function that checks if a stack is empty!
isEmpty :: State [a] Bool
isEmpty = null <$> get
-- isEmpty = do
--   st <- get
--   return (null st)

-- Return the top element of the stack if it is not empty.
top :: State [a] (Maybe a)
top = do
  st <- get
  case st of
    [] -> return Nothing
    (a:as) -> return (Just a)

-- Remove and return the top element of the stack if it is not empty.
pop :: State [a] (Maybe a)
pop = do
  st <- get
  case st of
    [] -> return Nothing
    (a:as) -> do
      put as
      return (Just a)


-- runState stackTest [] == ('d', "ba")
stackTest :: State [Char] (Maybe Char)
stackTest = do
  push 'a'
  push 'b'
  push 'c'
  pop
  push 'd'
  pop



-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs xs = evalState (go xs) 0 where
  go :: [Int] -> State Int [Int]
  go [] = return []
  go (a:as) = do
    m <- get
    let b = max a m
    put b
    as' <- go as
    return (b : as')

    -- if a > m then do
    --   put a
    --   as' <- go as
    --   return (a : as')
    -- else do
    --   as' <- go as
    --   return (m : as')
