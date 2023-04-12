{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy06 where

import Control.Monad

-- Előző óráról maradt
------------------------------------------

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3' :: IO ()
io3' = do
  l <- getLine
  if elem 'x' l
    then do
      putStrLn l
    else do
      io3'
      putStrLn l

-- Azonos sorrendben
io3'' :: IO ()
io3'' = go []
  where
    go :: [String] -> IO ()
    go sl = do
      l <- getLine
      if not $ elem 'x' l
        then go (l:sl)
        else do
          mapM_ putStrLn (reverse (l:sl))
          -- putStrLn l


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
modify f = do
  st <- get
  put (f st)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

-- típus (2 paraméteres):   State :: * -> * -> *
--  State s a  :   mellékhatás: egy darab "s" típus írható/olvasható referencia
--     instance Monad (State s)
--     "a": visszatérési érték típusa (mint minden monádban)

-- futtatás:
--------------------------------------------------------------------------------

--                       kezdő állapot      (érték, végső állapotot)
-- runState :: State s a ->      s          -> (a, s)

-- (kényelmi függvények)
-- evalState :: State s a ->      s          -> a
-- execState :: State s a ->      s          -> s


--------------------------------------------------------------------------------

-- modify :: (s -> s) -> State s ()
-- modify f = do
--   st <- get
--   put (f st)

triple :: State Int ()
triple = modify (*3)
-- triple = do
--   n <- get
--   put (n*3)

addTwo :: State Int ()
addTwo = modify (+2)
-- addTwo = do
--   n <- get
--   put (n+2)

-- TODO: Auxiliary function!

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

-- Examples:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Define a function that checks if a stack is empty!
isEmpty :: State [a] Bool
isEmpty = fmap null get
-- isEmpty = do
--   st <- get
--   return $ null st
  -- case st of
  --   [] -> return True
  --   a : as -> return False



-- Return the top element of the stack if it is not empty.
top :: State [a] (Maybe a)
-- top = do
--   st <- get
--   b <- isEmpty
--   if b then return Nothing else return (Just (head st))
top = do
  st <- get
  if null st
    then do
      return Nothing
    else do
      return (Just (head st))

-- Remove and return the top element of the stack if it is not empty.
pop :: State [a] (Maybe a)
pop = do
  st <- get
  if null st
    then do
      return Nothing
    else do
      put (tail st)
      return (Just (head st))


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
  go (x:xs) = do
    st <- get
    if x > st
      then do
        put x
        xs' <- go xs
        return (x:xs')
      else do
        xs' <- go xs
        return (st:xs')


-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show)

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined


-- Írd át a következő függvényeket úgy, hogy *csak* a (State :: (s -> (a, s)) ->
-- State s a) konstruktort használd, monád/funktor instance-t és
-- get/put/modify-t ne használj.
--------------------------------------------------------------------------------

modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

  -- Művelet végrehajtása lokálisan: az állapot visszaáll a művelet után.
locally :: State s a -> State s a
locally ma = do
  s <- get        -- "elmentjük" az állapotot
  a <- ma         -- futtatjuk ma-t
  put s           -- visszaállítjuk
  pure a

-- Állapot módosítás n-szer
-- modifyNTimes :: Int -> (s -> s) -> State s ()
-- modifyNTimes 0 f = pure ()
-- modifyNTimes n f = modify f >> modifyNTimes (n - 1) f


--------------------------------------------------------------------------------

-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined