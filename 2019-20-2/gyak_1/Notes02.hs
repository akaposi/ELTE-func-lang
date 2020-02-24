
data Either' a b = Left' a | Right' b

-- feladat:
--   - Eq instance Either'-re
--   - Show instance Either'-re

instance (Eq a, Eq b) => Eq (Either' a b) where
  Left' a  == Left' a'  = a == a'
  Right' b == Right' b' = b == b'
  _        == _         = False    -- konstruktorok különböznek

instance (Show a, Show b) => Show (Either' a b) where
  show (Left' a)  = "Left' " ++ show a
  show (Right' b) = "Right' " ++ show b


-- gyakfeladatok01.hs-ból
------------------------------------------------------------

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)
-- f1 abcd = (snd (fst abcd), snd (snd (snd abcd)))

-- standard: ($)
f2 :: (a -> b) -> a -> b
f2 = ($)
-- f2 f a = f a

-- standard: (.)
f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = (.)
-- f3 f g a = f (g a)

-- standard: flip
f4 :: (a -> b -> c) -> b -> a -> c
f4 = flip
-- f4 f b a = f a b

-- standard: curry
f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = curry
-- f5 f a b = f (a, b)

-- standard: uncurry
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry f (a, b) = f a b

f6 :: (a -> (b, c)) -> ((a -> b), (a -> c))
f6 f = ((\a -> fst (f a)), (\a -> snd (f a)))

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) a = (f a, g a)

f8 :: (Either a b -> c) -> (a -> c, b -> c)
f8 f = (\a -> f (Left a), \b -> f (Right b))

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f, g) (Left a)  = f a
f9 (f, g) (Right b) = g b

-- bónusz feladat (nehéz)
f10 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f10 f g = f (g (\a -> f a a)) (g (\a -> f a a))
