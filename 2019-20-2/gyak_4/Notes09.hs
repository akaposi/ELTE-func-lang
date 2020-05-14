{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Notes09 where
import Control.Monad.State
import Data.Foldable

-- class Functor m => Monad' m where
--   return' :: a -> m a
--   bind'   :: m a -> (a -> m b) -> m b

-- newtype State' s a = State' { runState :: s -> (a, s) }
--                    deriving (Functor)

-- instance Monad' (State' s) where
--   return' = undefined
--   bind'   = undefined

-- get' :: State' s s
-- get' = State' $ \s -> (s, s)

-- put' :: s -> State' s ()
-- put' s = State' $ \_ -> ((), s)

-- modify' :: (s -> s) -> State' s ()
-- modify' f = do
--   s <- get'
--   put' (f s)


data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Eq, Ord, Functor)

-- labelLeaf should be used in labelTree' to compute the label of a leaf
labelLeaf :: State Int Int
labelLeaf = do
  i <- get             -- i = state
  modify (+ 1)         -- state += 1
  -- or:  put (i + 1)
  return i             -- return i

-- labelTree' labels a tree, starting at the current state of the state monad
--   (State Int a)
labelTree' :: Tree a -> State Int (Tree Int)
labelTree' (Leaf _)   = Leaf <$> labelLeaf
labelTree' (Node l r) = Node <$> labelTree' l <*> labelTree' r

-- labelTree (Node (Leaf ()) (Leaf ())) == Node (Leaf 0) (Leaf 1)
-- labelTree (Leaf ()) == Leaf 0
-- labelTree (Node (Node (Leaf ()) (Leaf ())) (Leaf ()))
--         == Node (Node (Leaf 0)  (Leaf 1))  (Leaf 2)
labelTree :: Tree a -> Tree Int
labelTree t = evalState (labelTree' t) 0


-- instance Foldable Tree where
--   -- Traversable requires Foldable, do we define an empty instance of Foldable Tree
-- instance Traversable Tree where
--   -- traverse :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
--   -- traverse for lists is mapM or forM
--   traverse = undefined

-- Define traverse and use it to redefine labelTree
-- Note that we can derive Traversable instances

-- labelTree'' :: Tree a -> State Int (Tree Int)
-- labelTree'' = traverse (\_ -> labelLeaf)
