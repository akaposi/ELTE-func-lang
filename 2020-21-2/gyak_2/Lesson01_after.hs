{-# options_ghc -Wincomplete-patterns #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

{-
  ghci commands:
    - :l(oad) file
    - :r(eload)
    - :bro(wse)
    - :t(ype) expression
    - :i(nfo) definition

  examples:
    - :load Lesson1.hs
    - :t foo
    - :i (+)
-}

module Lesson01 where

{- Algebraic Data Types (ADT) -}

-- Bool
-- data Bool = True | False
--
-- a :: Bool
-- a = True

-- Color
data Color = Red | Green | Blue deriving (Show)

c :: Color
c = Red

-- Pair
data Pair a b = MakePair a b

p :: Pair Color Bool
p = MakePair Blue False

data PairInt = MakePairInt Int Int

pi :: PairInt
pi = MakePairInt 5 7


-- List
data List a = Nil | Cons a (List a)

l :: List Bool
l = Cons True (Cons False (Cons True Nil))

infixr 5 -+-
(-+-) :: a -> List a -> List a
(-+-) a as = Cons a as

l' :: List Color
l' = Red -+- Green -+- Red -+- Nil

l'' :: List Color
l'' = Blue -+- Blue -+- Green -+- Nil

-- Tree
data Tree = Leaf | Node Tree Tree

--    ·
--   / \
--  ·   ·
--     / \
--    ·   ·

t :: Tree
t = Node Leaf (Node Leaf Leaf)

--      ·
--     / \
--    ·   ·
--   / \
--  ·   ·
--     / \
--    ·   ·

t' :: Tree
t' = Node (Node Leaf (Node Leaf Leaf)) Leaf

{- Functions -}

xor :: Bool -> Bool -> Bool
-- xor = (/=)
-- xor a b = a /= b
xor False b = b
xor True  False = True
xor True  True  = False
-- xor False False = False
-- xor False True  = True
-- xor True  False = True
-- xor True  True  = False

-- 1, 2, Nil `conc` 4, 5, Nil

-- conc (Cons 1 (Cons 2 Empty)) (Cons 4 (Cons 4 Empty))
-- Cons 1 (conc (Cons 2 Empty) (Cons 4 (Cons 5 Empty)))
-- Cons 1 (Cons 2 (conc (Empty) (Cons 4 (Cons 5 Empty))))
-- Cons 1 (Cons 2 (Cons 4 (Cons 5 Empty)))

l1 :: List Int
l1 = 1 -+- 2 -+- Empty

l2 :: List Int
l2 = 4 -+- 5 -+- Empty

conc :: List a -> List a -> List a
conc Nil ys = ys
conc (Cons x xs) ys = Cons x (conc xs ys)

height :: Tree -> Int
height Leaf = 0
height (Node l r) = 1 + (max (height l) (height r))

{- Type Classes -}

class Eq' a where
  eq' :: a -> a -> Bool

instance Eq' Color where
  eq' Red   Red   = True
  eq' Green Green = True
  eq' Blue  Blue  = True
  eq' _     _     = False

-- -- In std: Ord [(<), (>), (<=), (>=)]
class Eq' a => Ord' a where
  lte :: a -> a -> Bool

instance Ord' Color where
  lte Red   _     = True
  lte Green Green = True
  lte _     Blue  = True
  lte _     _     = False
  -- lte Red   _    = True
  -- lte Green Red  = False
  -- lte Green _    = True
  -- lte Blue  Blue = True
  -- lte _     _    = False


-- class Show' a where
--   show' :: a -> String

instance Show a => Show (List a) where
  show Nil = ""
  show (Cons x Nil) = show x
  show (Cons x xs) = show x ++ ", " ++ (show xs)

--
-- -- Define the following instances:
-- instance Eq' Color where
--   eq = undefined
--
-- instance Ord' Color where
--   lte = undefined
--
-- instance Show' Color where
--   show' = undefined
--
-- instance Eq' Pair where
--   eq = undefined
--
-- instance Ord' Pair where
--   lte = undefined
--
-- instance Show' Pair where
--   show' = undefined
--
-- --
--
-- instance Eq' a => Eq' (Maybe a) where    -- standard: data Maybe a = Nothing | Just a
--   eq = undefined
--
-- instance Ord' a => Ord' (Maybe a) where   -- Nothing < Just x
--   lte = undefined
--
-- instance Show' a => Show' (Maybe a) where
--   show' = undefined
--
-- instance Eq' a => Eq' [a] where
--   eq = undefined
--
-- instance Ord' a => Ord' [a] where
--   lte = undefined
--
-- instance Show' a => Show' [a] where
--   show' = undefined
--
-- -- instance Eq' a => Eq' (Tree a) where
-- --   eq = undefined
-- --
-- -- instance Ord' a => Ord' (Tree a) where
-- --   lte = undefined
-- --
-- -- instance Show' a => Show' (Tree a) where
-- --   show' = undefined
--
--
-- Define the following functions in a type correct and total manner.
-- (No infinite loops or exceptions allowed.)

f1 :: (a, (b, (c, d))) -> (b, c)
f1 (x, (y, (z, zs))) = (y, z)

f2 :: (a -> b) -> a -> b
f2 f a = f a
--
-- f3 :: (b -> c) -> (a -> b) -> a -> c
-- f3 = undefined
--
-- f4 :: (a -> b -> c) -> (b -> a -> c)
-- f4 = undefined
--
-- f5 :: ((a, b) -> c) -> (a -> b -> c)
-- f5 = undefined
--
-- f6 :: (a -> (b, c)) -> (a -> b, a -> c)
-- f6 = undefined
--
asdf :: Maybe Int -> Int
asdf a = case a of
           Just a | a > 10 -> 1
           Just a -> 5
           Nothing -> 10
--
-- f7 :: (a -> b, a -> c) -> (a -> (b, c))
-- f7 = undefined
--
-- data Either' a b = This a | That b
--
-- data Error = EmptyList | DivideByZero
--
-- head' :: List a -> Either' a Error
-- head' Nil = That EmptyList
-- head' (Cons x xs) = This x

-- data Maybe a = Just a | Nothing

--
-- f8 :: (Either a b -> c) -> (a -> c, b -> c)
-- f8 = undefined
--
f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 (f, g) (Left a) = f a
f9 (f, g) (Right b) = g b
--
-- f10 :: Either (a, b) (a, c) -> (a, Either b c)
-- f10 = undefined
--
-- f11 :: (a, Either b c) -> Either (a, b) (a, c)
-- f11 = undefined
--
-- -- Extra (harder) task:
-- f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
-- f12 = undefined
