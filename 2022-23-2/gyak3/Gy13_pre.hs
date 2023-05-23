{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy13 where

import Control.Applicative
import Control.Monad
import Data.Char

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data List a b
  = Nil
  | Cons a b (List a b)
  deriving (Show)
-----------------------------------
-- Értelemszerűen kell ezeket megadni.

instance (Eq a, Eq b) => Eq (List a b) where
  (==) = undefined

instance Functor (List a) where
  fmap = undefined

instance Foldable (List a) where
  foldr = undefined

instance Traversable (List a) where
  traverse = undefined

-- Írj egy függvényt, ami két listára bont egy List a b-t!
unpack :: List a b -> ([a], [b])
unpack = undefined
{-
Pl.
unpack (Cons True 10 (Cons False 0 Nil)) == ([True, False], [10, 0])
unpack (Nil :: List Int Int) == ([], [])
unpack (Cons True False Nil) == ([True], [False])
-}

-- Írj egy függvényt, ami megfordítja az `a` típusú elemek sorrendjét a listában!
reverseAs :: List a b -> List a b
reverseAs = undefined
{-
Pl.
reverseAs (Cons True 10 (Cons False 0 Nil)) == Cons False 10 (Cons True 0 Nil)
reverseAs (Cons True False (Cons False True Nil)) == Cons False False (Cons True True Nil)
-}

data Tree a b
  = Leaf  a b
  | Node (Tree a b) (Tree a b)
  deriving (Eq, Show)

-- Írj egy függvényt, ami Tree a b-ből visszaad egy List a b-t,
-- amiben a levelek értékei vannak balról-jobbra bejárási sorrendben!
toList :: Tree a b -> List a b
toList = undefined
{-
Pl.
toList (Node (Leaf True 10) (Leaf False 20)) == Cons True 10 (Cons False 20 Nil)
toList (Leaf 20 30) == Cons 20 30 Nil
toList (Node (Leaf False 10) (Node (Leaf True 20) (Leaf True 30))) == Cons False 10 (Cons True 20 (Cons True 30 Nil))
-}

-- Írj egy függvényt State monád használatával,
-- ami egy Tree a b-ben az a típusú értékeket beszámozza balról-jobbra bejárási sorrendben!
-- A számozás 0-ról induljon.
labelAs :: Tree a b -> Tree (a, Int) b
labelAs = undefined
{-
Pl.
labelAs (Node (Leaf True 10) (Leaf False 20)) == Node (Leaf (True,0) 10) (Leaf (False,1) 20)
labelAs (Leaf True True) == Leaf (True,0) True
labelAs (Node (Leaf False 10) (Node (Leaf True 20) (Leaf True 30))) == Node (Leaf (False,0) 10) (Node (Leaf (True,1) 20) (Leaf (True,2) 30))
-}

--------------------------------------------------------------------------------
data BinaryTree a = BinaryLeaf a | BinaryNode (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

instance Functor BinaryTree where
    fmap = undefined

instance Foldable BinaryTree where
    foldr = undefined

instance Traversable BinaryTree where
    -- traverse :: Applicative f => (a -> f b) -> Binary a -> f (Binary b)
    traverse = undefined

data RoseTree a = Branch a [RoseTree a]
  deriving (Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 [] -- Leaf x ~ Branch x []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

-- Írd meg az alábbi instance-okat!
instance Eq a => Eq (RoseTree a) where
  (==) = undefined

instance Functor RoseTree where
  fmap = undefined

instance Foldable RoseTree where
  foldr   = undefined

instance Traversable RoseTree where
  traverse = undefined

-- Add vissza az "a" típusú elemek számát egy fában!
countElems :: RoseTree a -> Int
countElems = undefined

-- Add vissza a maximális "a" értéket egy fából!
maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

-- Számozd be bal-jobb bejárási sorrendben a fát!
label :: RoseTree a -> RoseTree (a, Int)
label = undefined

-- Írj egy függvényt, ami egy fában az összes "n :: Int" értéket kicseréli az
-- adott "[a]" lista n-edik elemére! Ha "n" bárhol nagyobb vagy egyenlő mint a
-- lista hossza, akkor legyen a végeredmény Nothing.
transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined