{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}

module Notes04 where
import Control.Monad (ap)

-- class Functor f => Applicative f where ...

-- class Applicative f => Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
-- (>>=) is read "bind".

-- Functions related to the Monad instance for lists:
mapList :: (a -> b) -> [a] -> [b]
mapList f xs = [ f x | x <- xs ]

returnList :: a -> [a]
returnList x = [x]

bindList :: (a -> [b]) -> [a] -> [b]
bindList f xs = [ y | x <- xs, y <- f x ]

-- data Maybe a = Nothing | Just a

-- The type Maybe a correponds to lists of length <= 1.
--    Nothing   ~   []
--    Just x    ~   [x]

-- (a -> b)       is the type of functions from a to b
-- (a -> Maybe b) is the type of functions from a to b that can also fail (return Nothing)

-- Bonus question: What is (a -> [b]) ?

returnMaybe :: a -> Maybe a
returnMaybe = undefined

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined
-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

-- Already defined in Prelude:
--   instance Monad Maybe where
--     return  = returnMaybe
--     m >>= f = bindMaybe f m

-- All of the remaining functions in this file can be defined 
--   using returnList/returnMaybe, mapList/mapMaybe and bindList/bindMaybe.
-- You can also try to define these functions directly.

-- join :: Monad m => m (m a) -> m a

concatList :: [[a]] -> [a]
concatList = undefined

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe = undefined

-- ap :: Monad m => m (a -> b) -> m a -> m b

apList :: [a -> b] -> [a] -> [b]
apList = undefined
-- example:
--   apList [ (*2), (*3), (*5) ] [ 1, 7 ] == [ (*2) 1, (*2) 7, (*3) 1, (*3) 7, (*5) 1, (*5) 7 ]
--                                        == [ 2, 14, 3, 21, 5, 35 ]

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe = undefined

-- sequence :: Monad m => [m a] -> m [a]

sequenceList :: [[a]] -> [[a]]
sequenceList = undefined
-- examples:
--   sequenceList [[1, 2], [3, 4]] == [[1,3],[1,4],[2,3],[2,4]]

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = undefined
-- examples:
--   sequenceMaybe [] = Just []
--   sequenceMaybe [Nothing] = Nothing
--   sequenceMaybe [Just 1, Just 2, Just 3] == Just [1, 2, 3]
--   sequenceMaybe [Just 1, Just 2, Nothing, Just 4] == Nothing

-- Other functions:
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined
-- examples:
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, -1, 3] == Nothing
--  filterMaybe (\x -> if x > 0 then Just (even x) else Nothing) [2, 1, 3] == [2]

data Tree a = Leaf a | Node (Tree a) (Tree a) 
            deriving (Eq, Show)
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined
-- examples:
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Leaf (-1)) 
--    == Nothing
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Node (Leaf 2) (Leaf 3)) 
--    == Just (Node (Leaf True) (Leaf False))

zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined
-- examples:
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [1, 3] 
--    == Nothing
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [2, 3] 
--    == [1, 5]