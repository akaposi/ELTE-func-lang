{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}

module Notes07 where

import Control.Monad

-- State monad
-- (We could also import Control.Monad.State)

newtype State s a = State { runState :: s -> (a, s) } 
                  deriving(Functor)

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
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
-- Labelling trees using the State monad

data BinTree a = Leaf a 
               | Node (BinTree a) (BinTree a)
               deriving(Eq, Ord, Show, Functor)

-- The function labelTree should label the leaves of a tree with increasing integers:
--    labelTree (Leaf ()) == Leaf 0
--    labelTree (Node (Leaf ()) (Leaf ())) == Node (Leaf 0) (Leaf 1)
--    labelTree (Node (Leaf ()) (Node (Leaf ()) (Leaf ()))) == Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
--    ..

-- Hint: define a function labelTree_State :: BinTree a -> State Int (BinTree Int), 
--   where the state represents the next leaf value.
-- labelTree_State should be defined by recursion on its argument.
labelTreeState :: BinTree a -> State Int (BinTree Int)
labelTreeState (Leaf _)   = do
  i <- get
  put (i+1)
  pure (Leaf i)
labelTreeState (Node l r) = do
  l' <- labelTreeState l
  r' <- labelTreeState r
  pure (Node l' r')
-- labelTreeState (Node l r) = Node <$> labelTreeState l <*> labelTreeState r

-- labelTree should be defined using evalState and labelTree_State
labelTree :: BinTree a -> BinTree Int
labelTree t = evalState (labelTreeState t) 0

-- The function labelTreeMax should label the leaves of a tree with the maximum leaf value 
--        to the left of it (you can assume that all values are positive).
--    labelTreeMax (Leaf 10) == Leaf 10
--    labelTreeMax (Node (Leaf 10) (Leaf 100)) == Node (Leaf 10) (Leaf 100)
--    labelTreeMax (Node (Leaf 100) (Leaf 10)) == Node (Leaf 100) (Leaf 100)
--    labelTreeMax (Node (Leaf 2) (Node (Leaf 1) (Leaf 3))) == Node (Leaf 2) (Node (Leaf 2) (Leaf 3))
--    ..

labelTreeMaxState :: BinTree Int -> State Int (BinTree Int)
labelTreeMaxState (Leaf x)   = do
  modify (max x)
  Leaf <$> get
labelTreeMaxState (Node l r) = do
  l' <- labelTreeMaxState l
  r' <- labelTreeMaxState r
  pure (Node l' r')

labelTreeMax :: BinTree Int -> BinTree Int
labelTreeMax t = evalState (labelTreeMaxState t) 0


--------------------------------------------------------------------------------
-- Foldable and Traversable

-- Generalizations of `fmap :: Functor f => (a -> b) -> f a -> f b`:

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- mapM    :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- More general: traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)

-- Example: [] is Foldable and Traversable
foldMap_List :: Monoid m => (a -> m) -> [a] -> m
foldMap_List f []     = mempty
foldMap_List f (x:xs) = f x <> foldMap_List f xs

traverse_List :: Applicative m => (a -> m b) -> [a] -> m [b]
traverse_List f []     = pure []
traverse_List f (x:xs) = (:) <$> f x <*> traverse_List f xs

-- Define foldMap and mapM/traverse for BinTree

foldMapBinTree :: Monoid m => (a -> m) -> BinTree a -> m
foldMapBinTree f (Leaf x)   = f x
foldMapBinTree f (Node l r) = foldMapBinTree f l <> foldMapBinTree f r

traverseBinTree :: Applicative m => (a -> m b) -> BinTree a -> m (BinTree b)
traverseBinTree f (Leaf x)   = Leaf <$> f x
traverseBinTree f (Node l r) = Node <$> traverseBinTree f l <*> traverseBinTree f r

mapMBinTree :: Monad m => (a -> m b) -> BinTree a -> m (BinTree b)
mapMBinTree = traverseBinTree

labelTree' :: BinTree a -> BinTree Int
labelTree' t = evalState (traverseBinTree go t) 0
  where go _ = do i <- get
                  put (i+1)
                  pure i

instance Foldable BinTree where 
  foldMap = foldMapBinTree
instance Traversable BinTree where 
  traverse = traverseBinTree


-- Bonus: Define fmap and foldMap using traverse.

data Id a = Id { getId :: a}
instance Functor Id where fmap f (Id a) = Id (f a)
instance Applicative Id where pure = Id; Id f <*> Id a = Id (f a)

fmap' :: Traversable f => (a -> b) -> f a -> f b
fmap' f t = getId (traverse (Id . f) t)

data Writer m a = Writer { getWriter :: m }
instance Functor (Writer m) where fmap f (Writer x) = Writer x
instance Monoid m => Applicative (Writer m) where 
  pure _ = Writer mempty
  Writer a <*> Writer b = Writer (a <> b)

foldMap' :: (Traversable f, Monoid m) => (a -> m) -> f a -> m
foldMap' f t = getWriter (traverse (Writer . f) t)