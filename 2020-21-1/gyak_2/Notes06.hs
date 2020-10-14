{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes05 where

import Control.Monad (ap, forM, forM_, liftM2)

--------------------------------------------------------------------------------

-- Also in Control.Monad.State
newtype State s a = State { runState :: s -> (s, a) }
                  deriving(Functor)

execState :: State s a -> s -> s
execState (State f) s = fst (f s)

evalState :: State s a -> s -> a
evalState (State f) s = snd (f s)

put :: s -> State s ()
put s = State (\_ -> (s, ()))

get :: State s s
get = State (\s -> (s, s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> (f s, ()))

instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad (State s) where
  return x = State (\s -> (s, x))
  State f >>= g = State (\s -> let (s', a) = f s in runState (g a) s')

--------------------------------------------------------------------------------

-- Labelling trees using the State monad

data BinTree a = Leaf a 
               | Node (BinTree a) (BinTree a)
               deriving( Eq, Ord, Show, Functor )

-- The function labelTree should label the leaves of a tree with increasing integers:
--    labelTree (Leaf ()) == Leaf 0
--    labelTree (Node (Leaf ()) (Leaf ())) == Node (Leaf 0) (Leaf 1)
--    labelTree (Node (Leaf ()) (Node (Leaf ()) (Leaf ()))) == Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
--    ..

-- Hint: define a function labelTree_State :: BinTree a -> State Int (BinTree Int), 
--   where the state represents the next leaf value.
-- labelTree_State should be defined by recursion on its argument.
labelTree_State :: BinTree a -> State Int (BinTree Int)
labelTree_State (Leaf _)   = do
  x <- labelLeaf
  pure (Leaf x)
-- labelTree_State (Node l r) = liftM2 Node (labelTree_State l) (labelTree_State r)
-- labelTree_State (Node l r) = Node <$> labelTree_State l <*> labelTree_State r
labelTree_State (Node l r) = do
  l' <- labelTree_State l
  r' <- labelTree_State r
  pure $ Node l' r'

-- When reaching a leaf, we should use the current state as the leaf value and increment the state.
-- labelLeaf should increment the state by 1 and return the previous state.
labelLeaf :: State Int Int
labelLeaf = do
  x <- get
  modify (+1) -- or: put (x+1)
  return x

-- labelTree should be defined using evalState and labelTree_State
labelTree :: BinTree a -> BinTree Int
labelTree t = evalState (labelTree_State t) 0


-- The function labelTreeMax should label the leaves of a tree with the maximum leaf value 
--        to the left of it (you can assume that all values are positive).
--    labelTreeMax (Leaf 10) == Leaf 10
--    labelTreeMax (Node (Leaf 10) (Leaf 100)) == Node (Leaf 10) (Leaf 100)
--    labelTreeMax (Node (Leaf 100) (Leaf 10)) == Node (Leaf 100) (Leaf 100)
--    labelTreeMax (Node (Leaf 2) (Node (Leaf 1) (Leaf 3))) == Node (Leaf 2) (Node (Leaf 2) (Node Leaf 3))
--    ..
labelTreeMax_State :: BinTree Int -> State Int (BinTree Int)
labelTreeMax_State (Leaf x)   = Leaf <$> labelMaxLeaf x
labelTreeMax_State (Node l r) = Node <$> labelTreeMax_State l <*> labelTreeMax_State r

-- When reaching a leaf, we should use the current state as the leaf value and increment the state.
-- labelLeaf should increment the state by 1 and return the previous state.
labelMaxLeaf :: Int -> State Int Int
labelMaxLeaf x = do
  modify (\y -> max x y)
  get

labelTreeMax :: BinTree Int -> BinTree Int
labelTreeMax t = evalState (labelTreeMax_State t) 0


--------------------------------------------------------------------------------
-- Foldable and Traversable

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- mapM    :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- More general: traverse :: (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)

-- Example: [] is Foldable and Traversable
foldMap_List :: Monoid m => (a -> m) -> [a] -> m
foldMap_List f []     = mempty
foldMap_List f (x:xs) = f x <> foldMap_List f xs

mapM_List :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_List f []     = pure []
mapM_List f (x:xs) = (:) <$> f x <*> mapM_List f xs

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs f = mapM_List f xs

-- Define foldMap and mapM for BinTree

fmap_BinTree :: (a -> b) -> BinTree a -> BinTree b
fmap_BinTree f (Leaf x)   = Leaf (f x)
fmap_BinTree f (Node l r) = Node (fmap_BinTree f l) (fmap_BinTree f r)

foldMap_BinTree :: Monoid m => (a -> m) -> BinTree a -> m
foldMap_BinTree f (Leaf x)   = f x
foldMap_BinTree f (Node l r) = foldMap_BinTree f l <> foldMap_BinTree f r

mapM_BinTree :: Monad m => (a -> m b) -> BinTree a -> m (BinTree b)
mapM_BinTree f (Leaf x)   = Leaf <$> f x
mapM_BinTree f (Node l r) = Node <$> mapM_BinTree f l <*> mapM_BinTree f r

traverse_BinTree :: Applicative m => (a -> m b) -> BinTree a -> m (BinTree b)
traverse_BinTree f (Leaf x)   = Leaf <$> f x
traverse_BinTree f (Node l r) = Node <$> traverse_BinTree f l <*> traverse_BinTree f r

instance Foldable BinTree where foldMap = foldMap_BinTree
instance Traversable BinTree where 
  mapM     = mapM_BinTree
  traverse = traverse_BinTree

--------------------------------------------------------------------------------
-- We can use mapM_BinTree to redefine labelTree and labelTreeMax

labelTree' :: BinTree a -> BinTree Int
labelTree' t = evalState (mapM_BinTree (\_ -> labelLeaf) t) 0

labelTreeMax' :: BinTree Int -> BinTree Int
labelTreeMax' t = evalState (mapM_BinTree labelMaxLeaf t) 0

-- 
data Tree2 a = Leaf2 a | Node2 [Tree2 a]
             deriving (Show, Functor)
instance Foldable Tree2 where
  foldMap f (Leaf2 x) = f x
  foldMap f (Node2 xs) = foldMap (foldMap f) xs
instance Traversable Tree2 where
  traverse f (Leaf2 x) = Leaf2 <$> f x
  traverse f (Node2 xs) = Node2 <$> traverse (traverse f) xs