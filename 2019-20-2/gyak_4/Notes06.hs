{-# LANGUAGE MonadComprehensions #-}
module Notes06 where

-- Today : motivation for the Monad typeclass
--   Things shared by
--     []
--     Maybe

-- Both [] and Maybe are functors.
-- map :: (a -> b) -> [a] -> [b]

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

--   Maybe a    is similar to the type of lists of length <= 1

-- maybeToList returns a list of length <= 1
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- maybeFromList (maybeToList x) == x
maybeFromList :: [a] -> Maybe a
maybeFromList [] = Nothing
maybeFromList (x:_) = Just x

singletonList :: a -> [a]
singletonList x = [x]

singletonMaybe :: a -> Maybe a
singletonMaybe x = Just x
-- singletonMaybe x = maybeFromList (singletonList x)

concatList :: [[a]] -> [a]
concatList = concat

concatMaybe :: Maybe (Maybe a) -> Maybe a
concatMaybe (Just (Just x)) = Just x
concatMaybe Nothing         = Nothing
-- concatMaybe x = maybeFromList (concatList (fmap maybeToList (maybeToList x)))

-- List comprehensions
bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (x:xs) = f x ++ bindList f xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f xs = concat (fmap f xs)

l1 = [ (x,y)
     | x <- [1..2]
     , y <- [2..3]
     ]

l1' = bindList (\x ->
        bindList (\y ->
          singletonList (x,y)) [2..3]) [1..2]

l1'' = do
  x <- [1..2]
  y <- [2..3]
  singletonList (x,y)

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f Nothing  = Nothing
bindMaybe f (Just a) = f a

bindMaybe' :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe' f x = concatMaybe (fmap f x)

-- needs {-# LANGUAGE MonadComprehensions #-} at the top of the file
l3 = [ (x,y)
     | x <- Just 1
     , y <- Just 2
     ]

l3' = bindMaybe (\x ->
        bindMaybe (\y ->
          singletonMaybe (x,y)) (Just 2)) (Just 1)

l3'' = do
  x <- Just 1
  y <- Just 2
  singletonMaybe (x,y)

-- Define bindList and bindMaybe
-- Bonus : define bindList and bindMaybe, using concatList/concatMaybe , fmap

filterList :: (a -> Bool) -> [a] -> [a]
filterList = filter

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f (Just x) | f x = Just x
filterMaybe f x = x

l4 = [ (x,y)
     | x <- [1..2]
     , y <- [2..3]
     , odd (x + y)
     ]

l4' = bindList (\x ->
        bindList (\y ->
                     if odd (x+y) then
                       singletonList (x,y)
                     else []
                 ) [2..3]) [1..2]

l5 = [ (x,y)
     | x <- Just 1
     , y <- Just 2
     , odd (x + y)
     ]

l5' = bindMaybe (\x ->
        bindMaybe (\y ->
                      if odd (x+y) then
                        singletonMaybe (x, y)
                      else Nothing
                 ) (Just 1)) (Just 2)

-- Define l4, l5 using
--   bindList/bindMaybe
--   singletonList/singletonMaybe
--   filterList/filterMaybe

-- In some languages, bind is called concatMap
-- In haskell: (>>=)

helloworld' :: IO ()
helloworld' = [ ()
              | _ <- putStr "Hello "
              , _ <- putStr "World !\n"
              ]

helloworld :: IO ()
helloworld = do
  putStr "Hello "
  putStr "World !\n"
