{-# LANGUAGE DeriveFunctor, MonadComprehensions, KindSignatures #-}
module Notes03 where

import Control.Monad (ap)

-- class Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
-- (>>=) is read "bind".

returnList' :: a -> [a]
returnList' x = [x]

mapList' :: (a -> b) -> [a] -> [b]
mapList' f xs = [ f x | x <- xs ]

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f xs = xs >>= \x -> f x >>= \y -> return y
--               [ y | x <- xs, y <- f x ]
--               

concatList' :: [[a]] -> [a]
concatList' xss = xss >>= \xs -> xs >>= \x -> return x
  --              [ x | xs <- xss, x <- xs ]

-- If m is a Monad 
--   (a -> b)    : type of functions from a to b
--   (a -> m b)  : type of computations with inputs in a, outputs in b, 
--                         effects (failure, state) in m

-- Maybe a is "the type of lists of length <= 1"
--  Nothing  ~   []
--  Just x   ~   [x]

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)
             deriving(Show, Eq, Ord, Functor)
-- Functor can be derived

returnTree1 :: a -> Tree1 a
returnTree1 = Leaf1

-- mapList :: (a -> b)   -> [a] -> [b]
bindList   :: (a -> [b]) -> [a] -> [b]
bindList f xs = concat [ (f x) | x <- xs ]
-- bindList f []     = []
-- bindList f (x:xs) = f x ++ bindList f xs
-- bindList f xs = concatList (map f xs)
-- example: bindList (\x -> [x, x+1]) [1, 2] == [1, 2, 2, 3]

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f Nothing  = Nothing
bindMaybe f (Just x) = f x
-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 f (Leaf1 x) = f x
bindTree1 f (Node1 l r) = Node1 (bindTree1 f l) (bindTree1 f r)

instance Applicative Tree1 where pure = return; (<*>) = ap
instance Monad Tree1 where 
  return  = returnTree1
  t >>= f  = bindTree1 f t

tree1 :: Tree1 Int
tree1 = bindTree1 
        (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
        (Node1 (Leaf1 True) (Leaf1 False))

tree1' :: Tree1 Int
tree1' = Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))
-- tree1 == tree1'

concatList :: [[a]] -> [a]
concatList xs = bindList id xs

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe = bindMaybe id

concatTree1 :: Tree1 (Tree1 a) -> Tree1 a
concatTree1 = bindTree1 id

tree2 :: Tree1 (Tree1 Int)
tree2 = Node1 (Leaf1 (Node1 (Leaf1 0) (Leaf1 2))) (Leaf1 (Leaf1 3))
-- concatTree1 tree2 == Node1 (Node1 (Leaf1 0) (Leaf1 2)) (Leaf1 3)
-- tree2 = Node1 (Leaf1 x) (Leaf1 y) 
--   x = Node1 (Leaf1 0) (Leaf1 2)
--   y = Leaf1 3
-- concatTree1 tree2 == Node1 x y

-- ap :: Monad m => m (a -> b) -> m a -> m b

apList :: [a -> b] -> [a] -> [b]
apList [] xs = []
apList (f:fs) xs = map f xs ++ apList fs xs

apList fs xs = [ f x | f <- fs, x <- xs ]
apList fs xs = fs >>= \f -> map f xs
-- example:
--   apList [ (*2), (*3), (*5) ] [ 1, 7 ] == [ (*2) 1, (*2) 7, (*3) 1, (*3) 7, (*5) 1, (*5) 7 ]
--                                        == [ 2, 14, 3, 21, 5, 35 ]

apMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apMaybe mf ma = do
  f <- mf
  a <- ma
  pure (f a)

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mf ma = do
  f <- mf
  a <- ma
  pure (f a)

-- sequence :: Monad m => [m a] -> m [a]
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = pure []
sequence' (mx:mxs) = do
  x <- mx
  xs <- sequence' mxs
  pure (x:xs)

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = sequence'
-- examples:
--   sequenceMaybe [] = Just []
--   sequenceMaybe [Nothing] = Nothing
--   sequenceMaybe [Just 1, Just 2, Just 3] = Just [1, 2, 3]
--   sequenceMaybe [Just 1, Just 2, Nothing, Just 4] = Nothing

-- (<$>) = fmap
-- (<*>) = ap
traverseList_Maybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseList_Maybe f [] = pure []
traverseList_Maybe f (x:xs) = (:) <$> f x <*> traverseList_Maybe f xs

traverseTree1_Maybe :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
traverseTree1_Maybe f (Leaf1 x) = Leaf1 <$> f x
traverseTree1_Maybe f (Node1 l r) = Node1 <$> traverseTree1_Maybe f l <*> traverseTree1_Maybe f r


-- binary trees are like lists
--   Node1 (Leaf1 0) (Node1 (Leaf1 1) (Leaf1 2))    ~    [0, (1, 2)]

sequenceTree1 :: [Tree1 a] -> Tree1 [a]
sequenceTree1 = sequence'
-- sequenceTree1 [] = Leaf1 []
-- sequenceTree1 [Leaf1 0] == Leaf1 [0]
-- sequenceTree1 [Leaf1 0, Leaf1 1] == Leaf1 [0, 1]
-- sequenceTree1 [Node1 (Leaf1 'L') (Leaf1 'R'), Node1 (Leaf1 'L') (Leaf1 'R')] 
--     == Node1 (Node1 (Leaf1 "LL") (Leaf1 "LR")) (Node1 (Leaf1 "RL") (Leaf1 "RR"))

