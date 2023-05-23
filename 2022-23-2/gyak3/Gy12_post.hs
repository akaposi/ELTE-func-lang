{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy12 where

import Control.Monad
import Control.Applicative
import Debug.Trace

-- State
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

execState :: State s a -> s -> s
execState sta s = snd (runState sta s)


-- Könnyebb
--------------------------------------------------------------------------------

data MaybeTree a
  = Node (MaybeTree a) (Maybe a) (MaybeTree a)
  | Leaf a
  deriving (Show)

exMaybeTree :: MaybeTree Int
exMaybeTree = Node (Node (Leaf 3) Nothing (Leaf 2)) (Just 5) (Leaf 6)

-- Írd meg az instance-okat!
instance (Eq a) => Eq (MaybeTree a) where
  (==) = undefined

instance Functor MaybeTree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (fmap f a) (fmap f r) 

instance Foldable MaybeTree where
  foldr f z (Leaf a) = f a z
  foldr f z (Node l a r) = foldr f (foldr f (foldr f z r) a) l

instance Traversable MaybeTree where
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node l a r) = Node <$> (traverse f l) <*> (traverse f a) <*> (traverse f r) 

-- Számold meg a tárolt `Just` konstruktorokat a fában.
countJusts :: MaybeTree a -> Int
countJusts = undefined

-- Add vissza csak a `Leaf`-ekben tárolt értékek listáját.
leaves :: MaybeTree a -> [a]
leaves = undefined

-- Egy fában told el az összes "a" elemet egy pozícióval jobbra, a balról-jobbra
-- bejárási sorrendben. A leginkább baloldali elem helyére kerüljön egy megadott
-- "default" érték.

-- Tipp: használd a `State a`-t a bejáráshoz! Az állapot legyen legutóbb bejárt
-- `a` típusú érték, vagy pedig az adott default érték, ha még nem jártunk be
-- egy elemet sem.

shiftElems :: a -> MaybeTree a -> MaybeTree a
shiftElems = undefined

-- példák a működésre:
--   shiftElems 10 (Leaf 0) == Leaf 10
--   shiftElems 10 (Node (Leaf 0) (Just 1) (Leaf 2)) == Node (Leaf 10) (Just 0) (Leaf 1)


-- Nehezebb
--------------------------------------------------------------------------------

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