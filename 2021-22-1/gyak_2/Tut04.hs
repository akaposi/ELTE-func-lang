{-# OPTIONS -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}
module Tut04 where

-- Maybe monad

--   (a -> b)       : "pure" functions
--   (a -> Maybe b) : functions from a to b that can fail

--    f :: a -> Maybe b
--       f x ~~> Just y    <-- `f x` has succeeded, and returned y
--       f x ~~> Nothing   <-- `f x` has failed

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
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = undefined

-- filter :: (a -> Bool) -> [a] -> [a]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x
                   then x : filter' p xs
                   else filter' p xs

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

-- examples:
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, -1, 3] == Nothing
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, 1, 3] == [2]

data Tree a = Leaf a 
            | Node (Tree a) (Tree a) 
            deriving (Eq, Show)

-- fmap :: (a -> b) -> Tree a -> Tree b
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x)   = Leaf (f x)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

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

-- Monad operations and combinators:

returnMaybe :: a -> Maybe a
returnMaybe = undefined

-- fmap :: (a -> b) -> (Maybe a -> Maybe b)
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined 

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe = undefined