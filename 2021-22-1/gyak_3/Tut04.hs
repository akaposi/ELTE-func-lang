{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Tut04 where

-- Maybe monad

-- Monad operations and combinators:

returnMaybe :: a -> Maybe a
returnMaybe = undefined

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe = undefined

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe = undefined

-- Exercises: 
--  1) Define the functions sequenceMaybe, mapMaybe, mapMaybeTree, filterMaybe, zipWithMaybe
--  2) Redefine these functions using the Maybe monad (bindMaybe)

sequenceId :: [a] -> [a]
sequenceId []     = []
sequenceId (x:xs) = x : sequenceId xs

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = undefined
-- examples:
--  sequenceMaybe [Just 0, Nothing, Just 1] = Nothing
--  sequenceMaybe [Just 0, Just 2, Just 1] = [0,2,1]


-- map :: (a -> b) -> [a] -> [b]
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = undefined

-- filter :: (a -> Bool) -> [a] -> [a]
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

-- examples:
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, -1, 3] == Nothing
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, 1, 3] == [2]

data Tree a = Leaf a | Node (Tree a) (Tree a) 
            deriving (Eq, Show)

-- fmap :: (a -> b) -> Tree a -> Tree b
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined
-- examples:
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Leaf (-1)) 
--    == Nothing
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Node (Leaf 2) (Leaf 3)) 
--    == Just (Node (Leaf True) (Leaf False))


-- zipWith :: (a -> b -> c) -> ([a] -> [b] -> [c])
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined
-- examples:
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [1, 3] 
--    == Nothing
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [2, 3] 
--    == [1, 5]

--------------------------------------------------------------------------------

kleisli :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Mabye c)
kleisli = undefined