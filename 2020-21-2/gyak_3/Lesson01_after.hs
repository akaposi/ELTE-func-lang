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
data Nem = Girl | Boy

a :: Nem
a = Girl

-- Color
data Color = Red | Green | Blue
--
c :: Color
c = Green

-- Pair

data IntBoolPair = MakeIntBoolPair Int Bool

ibp :: IntBoolPair
ibp = MakeIntBoolPair 5 False

data Pair a b = MakePair a b

isti :: Pair Nem Color
isti = MakePair Boy Green

-- List

-- infixr 5 :-:
-- data List a = Empty | a :-: (List a) deriving (Show)
--
-- l :: List Bool
-- l = True :-: False :-: True :-: Empty

data List a = Empty | Cons a (List a)

l :: List Bool
l = Cons True (Cons False (Cons True Empty))

infixr 5 -+-
(-+-) :: a -> List a -> List a
(-+-) = Cons

l' :: List Color
l' = Red -+- Blue -+- Red -+- Empty

l'' :: List Color
l'' = Green -+- Green -+- Blue -+- Empty

--

data BoolList = Empty' | Cons' Bool BoolList

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

-- xor :: Bool -> Bool -> Bool
-- xor False b = b
-- xor True  False = True
-- xor True  True  = False

xor :: Bool -> Bool -> Bool
xor = (/=)

-- conc (Cons 1 (Cons 2 Empty)) (Cons 3 (Cons 4 Empty))
-- Cons 1 (conc (Cons 2 Empty) (Cons 3 (Cons 4 Empty)))
-- Cons 1 (Cons 2 (conc (Empty) (Cons 3 (Cons 4 Empty))))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

l1 :: List Int
l1 = 1 -+- 2 -+- Empty

l2 :: List Int
l2 = 4 -+- 5 -+- Empty

conc :: List a -> List a -> List a
conc Empty ys = ys
conc xs Empty = xs
conc (Cons x xs) ys = Cons x (conc xs ys)

-- conc l1 l2
-- conc l' l''

height :: Tree -> Int
height Leaf = 0
height (Node l r) = 1 + max (height l) (height r)

{- Type Classes -}

class Eq' a where
  eq :: a -> a -> Bool

instance Eq' Color where
  eq Red Red = True
  eq Green Green = True
  eq Blue Blue = True
  eq _    _    = False

instance Show Color where
  show Red = "r"
  show Green = "g"
  show Blue = "b"

instance Show a => Show (List a) where
  show Empty = ""
  show (Cons x Empty) = show x
  show (Cons x xs) = show x ++ ", " ++ show xs

-- sort :: Ord a => List a -> List a

--
-- -- In std: Ord [(<), (>), (<=) (>=)]
-- class Eq' a => Ord' a where
--   lte :: a -> a -> Bool
--
-- class Show' a where
--   show' :: a -> String
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
-- -- Define the following functions in a type correct and total manner.
-- -- (No infinite loops or exceptions allowed.)
--
-- f1 :: (a, (b, (c, d))) -> (b, c)
-- f1 = undefined
--
-- f2 :: (a -> b) -> a -> b
-- f2 = undefined
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
-- f7 :: (a -> b, a -> c) -> (a -> (b, c))
-- f7 = undefined
--
-- f8 :: (Either a b -> c) -> (a -> c, b -> c)
-- f8 = undefined
--
-- f9 :: (a -> c, b -> c) -> (Either a b -> c)
-- f9 = undefined
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
