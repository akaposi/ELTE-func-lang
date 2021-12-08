
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
  fmap f (Branch a ts) = Branch (f a) (fmap (fmap f) ts)

instance Foldable RoseTree where
  -- foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr f b (Branch a ts) = f a (foldr (\t b -> foldr f b t) b ts)

     -- [t1, t2]
     -- foldr (\t b -> foldr f b t) b (t1:[]) ==
     --   (\t b -> foldr f b t) t1 b ==
     --   foldr f b t1

     -- foldr (\t b -> foldr f b t) b (t1:t2:[]) ==
     --   (\t b -> foldr f b t) t1 (foldr f b t2) ==
     --     foldr f (foldr f b t2) t1

     -- foldr f (foldr f (foldr f b t3) t2) t1
     -- foldr f (foldr f (foldr f (foldr f b t4) t3) t2) t1

     -- bináris elágazás foldr-je:
     -- foldr f b (Node l r) = foldr f (foldr f b r) l

  foldMap f (Branch a ts) = f a <> foldMap (foldMap f) ts

instance Traversable RoseTree where
  traverse f (Branch a ts) = Branch <$> f a <*> traverse (traverse f) ts

countElems :: RoseTree a -> Int
countElems = length    -- foldr (\_ num -> num + 1) 0

-- maxElem :: Ord a => RoseTree a -> a
-- maxElem = maximum

maxElem :: Ord a => RoseTree a -> a
maxElem (Branch a ts) = max a (maximum (map maxElem ts))

numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems t = evalState (traverse go t) 0 where
  go :: a -> State Int (a, Int)
  go a = do
    n <- get
    put $ n + 1
    pure (a, n)

safeIndex :: [a] -> Int -> Maybe a
safeIndex _      n | n < 0 = Nothing
safeIndex []     _ = Nothing
safeIndex (a:as) 0 = Just a
safeIndex (a:as) n = safeIndex as (n - 1)

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList as t = traverse (safeIndex as) t
