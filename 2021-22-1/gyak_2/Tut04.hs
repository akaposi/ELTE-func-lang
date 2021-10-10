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
sequenceMaybe []     = Just []

-- sequenceMaybe (x:xs) = x : sequenceMaybe xs
 -- x :: Maybe a
 -- sequenceMaybe xs :: Maybe [a]

-- sequenceMaybe (x:xs) = x `consMaybe` sequenceMaybe xs

sequenceMaybe (x:xs) = case (x, sequenceMaybe xs) of
  (Just x', Just xs') -> Just (x' : xs')
  _                   -> Nothing

-- return :: a -> Maybe a
-- return :: Monad m => a -> m a

sequenceMaybe' :: [Maybe a] -> Maybe [a]
sequenceMaybe' [] = return []
-- sequenceMaybe' (x:xs) = x >>= \x' ->
--                         sequenceMaybe xs >>= \xs' ->
--                         return (x':xs')

sequenceMaybe' (x:xs) = do
  x'  <- x
  xs' <- sequenceMaybe xs
  return (x':xs')

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe (Just x) (Just xs) = Just (x:xs)
consMaybe _        _         = Nothing

-- examples:
--  sequenceMaybe [Just 0, Nothing, Just 1] = Nothing
--  sequenceMaybe [Just 0, Just 2, Just 1] = Just [0,2,1]

-- map :: (a -> b) -> [a] -> [b]
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
-- mapMaybe f (x:xs) = f x `consMaybe` mapMaybe f xs
mapMaybe f (x:xs) = case (f x, mapMaybe f xs) of 
  (Just y, Just ys) -> Just (y : ys)
  _                 -> Nothing

mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe' f [] = return []
mapMaybe' f (x:xs) = do
  y <- f x
    -- f x  :: Maybe b
    -- y    :: b
  ys <- mapMaybe' f xs
  return (y : ys)

-- filter :: (a -> Bool) -> [a] -> [a]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x
                   then x : filter' p xs
                   else filter' p xs

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe p [] = Just []
filterMaybe p (x:xs) = case (p x, filterMaybe p xs) of 
  (Just b, Just ys) -> Just (if b
                             then x : ys
                             else ys)
  _ -> Nothing

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' p [] = return []
filterMaybe' p (x:xs) = do
  b <- p x
  ys <- filterMaybe p xs
  return (if b then x:ys else ys)

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
mapMaybeTree f (Leaf x) = do
  x' <- f x
  pure (Leaf x')
mapMaybeTree f (Node l r) = do
  l' <- mapMaybeTree f l
  r' <- mapMaybeTree f r
  pure (Node l' r')

-- examples:
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Leaf (-1)) 
--    == Nothing
--  mapMaybeTree (\x -> if x > 0 then Just (even x) else Nothing) (Node (Leaf 2) (Leaf 3)) 
--    == Just (Node (Leaf True) (Leaf False))


-- zipWith :: (a -> b -> c) -> ([a] -> [b] -> [c])
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f [] _ = pure []
zipWithMaybe f _ [] = pure []
zipWithMaybe f (x:xs) (y:ys) = do
  z <- f x y
  zs <- zipWithMaybe f xs ys
  pure (z:zs)

-- examples:
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [1, 3] 
--    == Nothing
--  zipWithMaybe (\x y -> if x + y > 0 then Just (x + y) else Nothing) [-1,2] [2, 3] 
--    == [1, 5]

--------------------------------------------------------------------------------

-- Monad operations and combinators:

-- class Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

returnMaybe :: a -> Maybe a
returnMaybe x = Just x

-- fmap :: (a -> b) -> (Maybe a -> Maybe b)
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f (Just x) = f x
bindMaybe f Nothing  = Nothing

-- join
-- concat :: [[a]] -> [a]
concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe (Just x) = x
concatMaybe _        = Nothing

concatMaybe' :: Maybe (Maybe a) -> Maybe a
concatMaybe' xss = bindMaybe id xss

bindMaybe' :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe' f xs = let y = fmap f xs in concatMaybe y


apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe f x = case (f, x) of 
  (Just f', Just x') -> Just (f' x')
  _ -> Nothing

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
apMaybe' :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe' f x = f >>= (\f' -> 
                 x >>= (\x' -> 
                   returnMaybe (f' x')
                 )
               )