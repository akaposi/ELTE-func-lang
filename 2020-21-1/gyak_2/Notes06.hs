{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes05 where

import Control.Monad (ap, forM, forM_)

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
labelTree :: BinTree a -> BinTree Int
labelTree = undefined

-- Hint: define a function labelTree' :: BinTree a -> State Int (BinTree Int), 
--   where the state represents the next leaf value.



-- The function labelTreeMax should label the leaves of a tree with the maximum leaf value 
--        to the left of it (you can assume that all values are positive).
--    labelTreeMax (Leaf 10) == Leaf 10
--    labelTreeMax (Node (Leaf 10) (Leaf 100)) == Node (Leaf 10) (Leaf 100)
--    labelTreeMax (Node (Leaf 100) (Leaf 10)) == Node (Leaf 100) (Leaf 100)
--    labelTreeMax (Node (Leaf 2) (Node (Leaf 1) (Leaf 3))) == Node (Leaf 2) (Node (Leaf 1) (Node Leaf 3))
--    ..
labelTreeMax :: BinTree Int -> BinTree Int
labelTreeMax = undefined


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

-- Define foldMap and mapM for BinTree

foldMap_BinTree :: Monoid m => (a -> m) -> BinTree a -> m
foldMap_BinTree = undefined

mapM_BinTree :: Monad m => (a -> m b) -> BinTree a -> m (BinTree b)
mapM_BinTree = undefined

instance Foldable BinTree where foldMap = foldMap_BinTree
instance Traversable BinTree where 
  mapM     = mapM_BinTree
  traverse = error "We don't define traverse now. Its definition should be identical to the definition of mapM."

--------------------------------------------------------------------------------
-- We can use mapM_BinTree to redefine labelTree and labelTreeMax

labelTree' :: BinTree a -> BinTree Int
labelTree' t = evalState (mapM_BinTree undefined t) 0

labelTreeMax' :: BinTree a -> BinTree Int
labelTreeMax' t = evalState (mapM_BinTree undefined t) 0
