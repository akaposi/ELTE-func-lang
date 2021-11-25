{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs #-}
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

data Either' a b = Left' a | Right' b | Both a b
  deriving (Eq, Show)

instance Functor (Either' c) where
  fmap :: (a -> b) -> Either' c a -> Either' c b
  fmap f (Left' a)  = Left' a                          -- konstruktor nem változik
  fmap f (Right' b) = Right' (f b)
  fmap f (Both a b) = Both a (f b)

instance Foldable (Either' c) where

  -- kettő közül elég az egyiket írni!

  foldr :: (a -> b -> b) -> b -> Either' c a -> b
  foldr f b (Left' a)   = b                -- nincs b érték
  foldr f b (Right' b') = f b' b           -- lásd: foldr f b [b'] == f b' b
  foldr f b (Both a b') = f b' b

  foldMap :: Monoid m => (a -> m) -> Either' c a -> m
  foldMap f (Left' a)  = mempty
  foldMap f (Right' b) = f b
  foldMap f (Both a b) = f b

-- listákra:
-- foldMap f []     = mempty
-- foldMap f (a:as) = f a <> foldMap f as       foldMap f [x, y, z] == f x <> f y <> f z <> mempty

instance Traversable (Either' c) where

  -- fmap + Applicative
  --   tipp: másoljuk az fmap definíciót, konstruktor alkalmazásokat írjuk át Applicative-ra
  traverse :: Applicative f => (a -> f b) -> Either' c a -> f (Either' c b)
  traverse f (Left' a)  = pure (Left' a)
  traverse f (Right' b) = Right' <$> f b
  traverse f (Both a b) = Both a <$> f b

  -- data Tree a = Leaf a | Leaf2 a a | Node (Tree a) (Tree a)
  -- traverse f (Leaf a)      = Leaf <$> f a
  -- traverse f (Node l r)    = Node <$> traverse f l <*> traverse f r
  -- traverse f (Leaf2 a1 a2) = Leaf2 <$> f a1 <*> f a2

partition :: [Either' a b] -> ([a], [b], [(a, b)])
partition []         = ([], [], [])
partition (eab:eabs) = case partition eabs of
  (as, bs, abs) -> case eab of
    Left' a  -> (a:as, bs  , abs       )
    Right' b -> (as  , b:bs, abs       )
    Both a b -> (as  , bs  , (a, b):abs)

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f (Both a b) : zipWith' f as bs
zipWith' f (a:as) []     = f (Left' a)  : zipWith' f as []
zipWith' f []     (b:bs) = f (Right' b) : zipWith' f [] bs
zipWith' f []     []     = []

-- kicsit hatékonyabban:
zipWith'' :: (Either' a b -> c) -> [a] -> [b] -> [c]
zipWith'' f (a:as) (b:bs) = f (Both a b) : zipWith'' f as bs
zipWith'' f (a:as) []     = f (Left' a)  : map (f . Left') as
zipWith'' f []     (b:bs) = f (Right' b) : map (f . Right') bs
zipWith'' f []     []     = []

traverseLeft' :: Applicative f => (a -> f b) -> Either' a c -> f (Either' b c)
traverseLeft' f (Left' a)  = Left' <$> f a
traverseLeft' f (Right' b) = pure (Right' b)
traverseLeft' f (Both a b) = Both <$> f a <*> pure b
                           -- (\a -> Both a b) <$> f a
                           -- flip Both b <$> f a

mapMaybeLeft :: (a -> Maybe b) -> [Either' a c] -> Maybe [Either' b c]
mapMaybeLeft f xs = traverse (traverseLeft' f) xs

-- mapMaybeLeft f = traverse (traverseLeft' f)
-- mapMaybeLeft = (traverseLeft' . traverseLeft')

-- (lásd: lens library-k (egymásba ágyazott bejáró függvények))


data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

-- levélbe: levéltől balra levő Int-ek összege
treeSums :: Tree Int -> Tree Int
treeSums t = evalState (go t) 0 where

  go :: Tree Int -> State Int (Tree Int)
  go (Leaf n) = do
    s <- get
    put (s + n)
    pure (Leaf s)

  go (Node l r) = Node <$> go l <*> go r

-- levélbe: levéltől balra levő Int-ek összege
treeSums' :: Tree Int -> Tree Int
treeSums' t = evalState (traverse go t) 0 where
  go :: Int -> State Int Int
  go n = do
    s <- get
    put (s + n)
    pure s


-- 10-11 pont
-- parser/interpreter : 9 pont
-- összesen: 20 pont
--   10-től 2-es
--   18-20  5-ös
--   16-17  4-es
