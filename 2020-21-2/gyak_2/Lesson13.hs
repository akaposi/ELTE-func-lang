{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.State
import Debug.Trace


{- Practice -}

-- # Rose Tree
-- https://en.wikipedia.org/wiki/Rose_tree

data RoseTree a = Branch a [RoseTree a]
                  deriving (Eq, Ord, Show)

rt :: RoseTree Int
rt =
  Branch 2
    [ Branch 5
      [ Branch 11 []
      ]
    , Branch 5 []
    , Branch 7
      [ Branch 11 []
      ]
    ]

rt' :: RoseTree Char
rt' =
  Branch 'f'
    [ Branch 'o'
      [ Branch 'o' []
      ]
    , Branch 'b' []
    , Branch 'a'
      [ Branch 'r' []
      ]
    ]

instance Functor RoseTree where
  fmap :: (a -> b) -> RoseTree a -> RoseTree b
  fmap f rt = undefined

instance Foldable RoseTree where
  foldr :: (a -> b -> b) -> b -> RoseTree a -> b
  foldr f acc rt = undefined

instance Traversable RoseTree where
  traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
  traverse f rt = undefined

removeDuplicates'' :: forall t a . (Traversable t, Eq a) => a -> t a -> t a
removeDuplicates'' blank t = evalState (traverse go t) [] where
  go :: a -> State [a] a
  go c = do
    prev <- get
    if c `elem` prev
       then pure blank
       else do
         put (c:prev)
         pure c


{- Example from earlier exam -}
-- https://github.com/AndrasKovacs/ELTE-func-lang/blob/master/2019-20-2/vizsga_minta/minta4/Feladatok.md

data Tree a = Leaf1 a | Leaf2 a a | Node (Tree a) (Maybe (Tree a))
  deriving (Eq, Ord, Show)

t :: Tree Int
t =
  Node
    (Leaf2 2 1)
    (Just (Node
      (Leaf1 10)
      (Just (Node
         (Leaf2 5 6)
         Nothing))))

t' :: Tree Char
t' =
  Node
    (Node
      (Leaf2 'f' 'o')
      (Just (Leaf1 'o')))
    (Just (Node
      (Leaf1 'b')
      (Just (Leaf2 'a' 'r'))))

-- Define the functor instance of this tree structure!
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f t = undefined

-- Define the foldable instance of this tree structure!
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f acc rt = undefined

-- Define a function that gets the leftmost element of a tree!
leftmost :: Tree a -> a
leftmost = undefined


-- Find the first value stored in a Leaf2 constructor that satisfies a criteria.
--
-- Examples:
-- + findInLeaf2 (==2) t == Just 2
-- + findInLeaf2 (==10) t == Nothing
-- + findInLeaf2 (>4) t == Just 5

findInLeaf2 :: (a -> Bool) -> Tree a -> Maybe a
findInLeaf2 = undefined


-- Define the traversable instance of this tree structure!
instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f rt = undefined


-- Label every value with the number of Leaf1 constructors to the left of it!
-- Hint: Use the imported State monad!
--
-- Examples:
--   countLeaf1s t ==
--     Node
--       (Leaf2 (2,0) (1,0))
--       (Just (Node
--         (Leaf1 (10,0))
--         (Just (Node (Leaf2 (5,1) (6,1)) Nothing))))
--
--   countLeaf1s (Node (Leaf2 1 2) (Just (Leaf2 3 4))) ==
--     Node (Leaf2 (1,0) (2,0)) (Just (Leaf2 (3,0) (4,0)))
--
--   countLeaf1s (Node (Leaf1 10) (Just (Node (Leaf1 20) (Just (Leaf1 30))))) ==
--     Node (Leaf1 (10,0)) (Just (Node (Leaf1 (20,1)) (Just (Leaf1 (30,2)))))

countLeaf1s :: Tree a -> Tree (a, Int)
countLeaf1s = undefined


-- Replace the values stored in Leaf2 constructors from a list of values!
--
-- Examples:
--   replaceLeaf2s [8,9,10,11] t ==
--     Node (Leaf2 8 9) (Just (Node (Leaf1 10) (Just (Node (Leaf2 10 11) Nothing))))
--
--   replaceLeaf2s [] t == t
--
--   replaceLeaf2s [8..20] t == replaceLeaf2s [8,9,10,11] t

replaceLeaf2s :: [a] -> Tree a -> Tree a
replaceLeaf2s = undefined
