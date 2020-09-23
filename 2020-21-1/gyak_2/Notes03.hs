{-# LANGUAGE DeriveFunctor, MonadComprehensions #-}
module Notes03 where

-- class Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
-- (>>=) is read "bind".

returnList' :: a -> [a]
returnList' x = [x]

mapList' :: (a -> b) -> [a] -> [b]
mapList' f xs = [ f x | x <- xs ]

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f xs = [ y | x <- xs, y <- f x ]

concatList' :: [[a]] -> [a]
concatList' xss = [ x | xs <- xss, x <- xs ]



data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)
             deriving(Show, Eq, Ord, Functor)

returnTree1 :: a -> Tree1 a
returnTree1 = Leaf1
-- Functor can be derived

bindList :: (a -> [b]) -> [a] -> [b]
bindList = undefined
-- example: bindList (\x -> [x, x+1]) [1, 2] == [1, 2, 2, 3]

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined
-- example:
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) Nothing      == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just True)  == Nothing
  -- bindMaybe (\x -> if x then Nothing else Just (not x)) (Just False) == Just True

bindTree1 :: (a -> Tree1 b) -> Tree1 a -> Tree1 b
bindTree1 = undefined


tree1 :: Tree1 Int
tree1 = bindTree1 
        (\x -> if x then Leaf1 0 else Node1 (Leaf1 0) (Leaf1 1))
        (Node1 (Leaf1 True) (Leaf1 False))

tree1' :: Tree1 Int
tree1' = Node1 (Leaf1 0) (Node1 (Leaf1 0) (Leaf1 1))

-- tree1 == tree1'



concatList :: [[a]] -> [a]
concatList = undefined

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe = undefined

concatTree1 :: Tree1 (Tree1 a) -> Tree1 a
concatTree1 = undefined

tree2 :: Tree1 (Tree1 Int)
tree2 = Node1 (Leaf1 (Node1 (Leaf1 0) (Leaf1 2))) (Leaf1 (Leaf1 3))

-- concatTree1 tree2 == Node1 (Node1 (Leaf1 0) (Leaf1 2)) (Leaf1 3)


sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = undefined

sequenceTree1 :: [Tree1 a] -> Tree1 [a]
sequenceTree1 = undefined


traverseTree1_Maybe :: (a -> Maybe b) -> Tree1 a -> Maybe (Tree1 b)
traverseTree1_Maybe = undefined