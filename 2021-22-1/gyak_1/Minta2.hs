
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

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

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

instance Functor RoseTree where
  fmap = undefined

instance Foldable RoseTree where
  foldr = undefined

countElems :: RoseTree a -> Int
countElems = undefined

maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

instance Traversable RoseTree where
  traverse = undefined

numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems = undefined

safeIndex :: [a] -> Int -> Maybe a
safeIndex = undefined

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined
