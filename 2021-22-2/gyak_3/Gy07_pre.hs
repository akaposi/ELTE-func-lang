{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy07 where

import Control.Monad
import Debug.Trace


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
modify f = do { st <- get ; put (f st) }

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


{- Stack -}

-- Add an element to the top of the stack by extending the state list!
push :: a -> State [a] ()
push a = modify (a:)

-- Remove and return the top element of the stack if it is not empty!
pop :: State [a] (Maybe a)
pop = do
  st <- get
  case st of
    [] -> return Nothing
    (a:as) -> do
      put as
      return (Just a)

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop = undefined


-- Trace + Memoization
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az addigi
-- elemek maximumára.  Tegyük fel, hogy a lista nem-negatív számokat tartalmaz.
-- Használd a (State Int)-et. Legyen az állapot a jelenlegi maximális Int.
maxs :: [Int] -> [Int]
maxs xs = evalState (go xs) 0 where
  go :: [Int] -> State Int [Int]
  go [] = return []
  go (a:as) = do
    m <- get
    if a > m then do
      put a
      as' <- go as
      return (a : as')
    else do
      as' <- go as
      return (m : as')

-- Traverse
maxs' :: [Int] -> [Int]
maxs' = undefined


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

exampleTree :: Tree String
exampleTree = Node
                (Node (Leaf "the") (Leaf "cake"))
                (Node (Leaf "is") (Node (Leaf "a") (Leaf "lie")))

{-
             ∙
            / \
           /   \
          /     \
         ∙       \
        / \       \
       /   \       \
   "the"   "cake"   ∙
                   / \
                  /   \
               "is"    ∙
                      / \
                     /   \
                   "a"   "lie"
-}

exampleTree' :: Tree Int
exampleTree' = Node (Node (Leaf 2) (Leaf 3)) (Leaf 5)

{-
        ∙
       / \
      ∙   5
     / \
    2   3
-}

-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!
-- pl: replaceLeaves [10, 20, 30] (Node (Leaf 2)  (Leaf 3))
--                              == Node (Leaf 10) (Leaf 20)
--     replaceLeaves [5] (Leaf 10)
--                    == (Leaf 5)
--     replaceLeaves [5] (Node (Leaf 0) (Node (Leaf 0) (Leaf 0)))
--                    == (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves = undefined

replaceLeavesSt :: [a] -> Tree a -> Tree a
replaceLeavesSt = undefined

replaceLeavesTr :: [a] -> Tree a -> Tree a
replaceLeavesTr = undefined


-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.
reverseElems :: Tree a -> Tree a
reverseElems = undefined

reverseElems' :: Tree a -> Tree a
reverseElems' = undefined


-- Értelmezd a következő utasítások listáját. Minden utasítás
-- egy Int-et módosító művelet. Az "Add n" adjon n-et a jelenlegi
-- állapothoz, a "Subtract n" vonjon ki n-t, és a "Mul" értelemszerűen.
data Op = Add Int | Subtract Int | Mul Int

evalOps :: [Op] -> State Int ()
evalOps = undefined

-- Add meg ennek segítségével az állapotot módosító (Int -> Int) függvényt.
runOps :: [Op] -> Int -> Int
runOps = undefined
