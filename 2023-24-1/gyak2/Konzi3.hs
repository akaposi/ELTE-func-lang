{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Konzi3 where

import Control.Monad
import Control.Applicative

{-
              V
fmap :: (a -> b) -> f a -> f b

??? :: (a -> b -> c) -> f a -> f b -> f c <- ilyet nem tud a Funktor

                        ^ --+-- ^   = ^
                       []     [1..] = ????

Funktor: prezervál struktúrát


Ötlet a Monád:
- Monád nem garantál struktúra prezerválást

                        ______
                         |   |
return :: Monad m => a -> m a
-- tiszta értéket berakunk monadikus környezetbe

(>>=) :: Monad m => m a -> (a -> m b) -> m b
                                 ^

IO monád
IO a = 'a' érték valami mellékhatással
(>>=) :: IO a -> (a -> IO b) -> IO b

-}

f :: IO ()
f = getLine >>= (\s1 -> getLine >>= (\s2 -> putStrLn (s1 ++ s2)))


-- Do notáció
f' :: IO ()
f' = do
  s1 <- getLine
  s2 <- getLine
  putStrLn (s1 ++ s2)


-- State monád, állapotváltozást reprezentál
newtype State s a = State { runState :: s -> (s, a) } deriving Functor

instance Applicative (State s) where
  pure a = State $ \s -> (s,a)
  (<*>) = ap

instance Monad (State s) where
  (State sa) >>= f = State $ \s -> let (s', a) = sa s in runState (f a) s'


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())

modify :: (s -> s) -> State s ()
modify f = do
  x <- get
  put (f x)

-- "Globális állapotunk" lesz
-- Singleton

g :: State [a] (Maybe a)
g = do
  x <- get
  case x of
    [] -> return Nothing
    (x : xs) -> do
      put xs
      return (Just x)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving (Functor, Foldable, Eq, Show)

-- State-el címkézzük meg az elemeket
labelTreeSt :: Tree a -> State Int (Tree (a, Int))
labelTreeSt (Leaf a) = do
  x <- get
  put (x + 1)
  return (Leaf (a, x))
labelTreeSt (Node tr1 a tr2) = do
  tr1' <- labelTreeSt tr1
  x <- get
  put (x + 1)
  tr2' <- labelTreeSt tr2
  return (Node tr1' (a, x) tr2')

--      V túl sok, ennyi nem szükséges
mapm :: Monad m => (a -> m b) -> [a] -> m [b]
mapm f [] = return []
mapm f (x : xs) = do
  x' <- f x         --- x'
  xs' <- mapm f xs  --- xs'
  return (x' : xs')


{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-}
-- Traversable: Művelet általánosítás

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f [] = pure []
mapA f (x : xs) = liftA2 (\x' xs' -> x' : xs') (f x) (mapA f xs)

-- (<$>) :: (a -> b) -> f a -> f b
-- b = b' -> c
-- (<$>) :: (a -> b' -> c) -> f a -> f (b' -> c)
-- f b'
-- (<*>) :: f (b' -> c) -> f b' -> f c
-- f <$> fb' <*> fc == liftA2 f fb' fc
-- f <$> fb' <*> fc <*> fd
-- f <$> fb' <*> fc <*> fd <*> fe
--             ^ ap


{-

param típusa
-- a :: a -> f a
-- a :: a-t tartalmazó kifejezés volt a típusa, de általában fmap f a
                                                                  ^
                                                             traverse f a
-- a :: Int pl -> a
                  pure a
instance Functor Tree where
   fmap :: (a -> b) -> Tree a -> Tree b

                          V <$>
   fmap f (Leaf a) = Leaf $ (f a)
   fmap f (Node tr1 a tr2) = Node $ (fmap f tr1) $ (f a) $ (fmap f tr2)
                 |                ^ <$>          ^ <*>   ^ <*>
                 |
-}

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node tr1 a tr2) = Node <$> traverse f tr1 <*> f a <*> traverse f tr2
