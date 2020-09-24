module Notes02 where

-- map in Prelude
mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = f x : mapList f xs
-- mapList f xs = [ f x | x <- xs ]

-- map (\x -> x) xs == xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

mapPair :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapPair f g (x, y) = (f x, g y)

-- mapPair id id x == x

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f g (Left x) = Left (f x)
mapEither f g (Right x) = Right (g x)

-- Define f and g using mapList, mapMaybe, mapPair, mapEither, ...
f1 :: (a -> b) -> [[a]] -> [[b]]
f1 f = mapList (mapList f)
-- f1 f xs = mapList (mapList f) xs
-- f1 f    = mapList (\x -> mapList f x)
-- f1 f xs = mapList (\x -> mapList f x) xs

f2 :: (a -> b) -> [[[a]]] -> [[[b]]]
f2 f = mapList (mapList (mapList f))

g1 :: (a -> b) -> [(a, a)] -> [(a, b)]
g1 f = mapList (mapPair id f)

data Tree1 a = Leaf1 a
             | Node1 (Tree1 a) (Tree1 a)
             deriving(Show, Eq, Ord)

mapTree1 :: (a -> b) -> Tree1 a -> Tree1 b
mapTree1 f (Leaf1 x) = Leaf1 (f x)
mapTree1 f (Node1 x y) = Node1 (mapTree1 f x) (mapTree1 f y)

instance Functor Tree1 where 
  fmap = mapTree1

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

mapTree2 :: (a -> b) -> Tree2 a -> Tree2 b
mapTree2 f (Leaf2 x)  = Leaf2 (f x)
mapTree2 f (Node2 xs) = Node2 (mapList (mapTree2 f) xs)

instance Functor Tree2 where 
  fmap = mapTree2

-- mapFun :: (a -> b) -> (Int -> a) -> (Int -> b)
mapFun :: (a -> b) -> (c -> a) -> c -> b
mapFun f g x = f (g x)
-- mapFun f g = (f . g)
-- mapFun f g = \x -> (f . g) x
-- mapFun = (.)
-- mapFun f = (f .)
-- mapFun f g x = (f . g) x

data Tree3 a = Leaf3 a
             | Node3 (Int -> [Tree3 a])

mapTree3 :: (a -> b) -> Tree3 a -> Tree3 b
mapTree3 f (Leaf3 x) = Leaf3 (f x)
mapTree3 f (Node3 x) = Node3 (mapFun (mapList (mapTree3 f)) x)

