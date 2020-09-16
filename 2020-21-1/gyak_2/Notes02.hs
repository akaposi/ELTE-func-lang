module Notes02 where

mapList :: (a -> b) -> [a] -> [b]
mapList = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair = undefined

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither = undefined

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)

mapTree1 :: (a -> b) -> Tree1 a -> Tree1 b
mapTree1 = undefined

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 = undefined