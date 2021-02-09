-- {-# options_ghc -Wincomplete-patterns #-}
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
-- data Bool = ?

-- a :: Bool
-- a = undefined

-- Color
-- data Color = ?
--
-- c :: Color
-- c = undefined

-- Pair
-- data Pair a b = MakePair a b

-- p :: Pair Color Bool
-- p = undefined

-- List
-- data List a = ?

-- l :: List Bool
-- l = undefined

-- l' :: List Bool
-- l' = undefined


-- Tree
-- data Tree = _

--    ·
--   / \
--  ·   ·
--     / \
--    ·   ·

-- t :: Tree
-- t = undefined

--      ·
--     / \
--    ·   ·
--   / \
--  ·   ·
--     / \
--    ·   ·

-- t' :: Tree
-- t' = undefined

{- Functions -}

-- xor : Bool -> Bool -> Bool
-- xor a b = undefined

-- concat : List a -> List a -> List a
-- concat xs ys = undefined

-- height : Tree -> Int
-- height t = undefined

{- Type Classes -}

-- class Eq' a where
--   eq :: a -> a -> Bool
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
