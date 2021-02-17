{-# options_ghc -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, ScopedTypeVariables, TypeApplications #-}

module Notes01 where

-- GHCi commands:
--  :l File.hs   Load the file File.hs
--  :r           Reload the loaded file
--  expr         Evaluate the expression `expr`
--  :t expr      Display the type of the expression `expr`
--  :i expr      Display information about the name `expr`

-- Typed holes:
--   (In vscode: place your cursor on _ and press F8 to 
--               display the expected type of a hole and
--               the types of the variables in scope)
--   (In GHCi: reload to see the same information)

-- Uncomment the following lines and try to press F8 on each hole!

-- ex0, ex1, ex2, ex3, ex4 :: ((a, b) -> c) -> b -> a -> c
-- ex0       = _
-- ex1 g b a = _
-- ex2 g b a = g _
-- ex3 g b a = g (_ , _)
-- ex4 g b a = g (a, b)

--------------------------------------------------------------------------------
-- Common Algebraic Data Types

-- (a, b) in Prelude
data Pair a b = Pair a b
              deriving (Show)

-- Either a b in Prelude
data Either' a b = Left' a
                 | Right' b
                 deriving (Show)

type EitherIntBool = Either' Int Bool
toInt :: EitherIntBool -> Int
toInt (Left' i)  = i
toInt (Right' b) = if b then 1 else 0

-- Maybe a in Prelude
data Maybe' a = Just' a 
              | Nothing'
              deriving (Show)

-- [a] in Prelude
data List a = Empty                -- []
            | Cons a (List a)      -- (x : xs)
            deriving (Show)

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show)

length'' :: [a] -> Int
length'' []     = 0
length'' (x:xs) = 1 + length'' xs

length' :: List a -> Int
length' Empty       = 0
length' (Cons x xs) = 1 + length' xs

height :: BinTree a -> Int
height (Leaf _)   = 0
height (Node l r) = 1 + max (height l) (height r)

--------------------------------------------------------------------------------

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) 
       -> Pair a b -> Pair a b -> Bool
eqPair eqA eqB (Pair a1 b1) (Pair a2 b2) = eqA a1 a2 && eqB b1 b2

eqEither :: (a -> a -> Bool) 
         -> (b -> b -> Bool) 
         -> Either' a b -> Either' a b -> Bool
eqEither eqA eqB (Left' a1)  (Left' a2)  = eqA a1 a2
eqEither eqA eqB (Right' b1) (Right' b2) = eqB b1 b2
eqEither eqA eqB (Left' a1)  (Right' a2) = False
eqEither eqA eqB (Right' a1) (Left' a2)  = False

eqEither' :: (a -> a -> Bool) 
         -> (b -> b -> Bool) 
         -> Either' a b -> Either' a b -> Bool
eqEither' eqA eqB (Left' a1)  (Left' a2)  = eqA a1 a2
eqEither' eqA eqB (Right' b1) (Right' b2) = eqB b1 b2
eqEither' eqA eqB _           _           = False

-- Maybe a ~ Either a ()

eqMaybe :: (a -> a -> Bool) -> Maybe' a -> Maybe' a -> Bool
eqMaybe eqA Nothing Nothing = True
eqMaybe eqA (Just x) (Just y) = x == y
eqMaybe eqA _ _ = Nothing

eqList :: (a -> a -> Bool) -> List a -> List a -> Bool
eqList eqA Empty         Empty         = True
eqList eqA (Cons a1 as1) (Cons a2 as2) = eqA a1 a2 && eqList eqA as1 as2
eqList eqA _              _            = False

eqBinTree :: (a -> a -> Bool) -> BinTree a -> BinTree a -> Bool
eqBinTree eqA (Leaf x)     (Leaf y)     = x == y
eqBinTree eqA (Node l1 r1) (Node l2 r2) = l1 == l2 && r1 == r2
eqBinTree eqA _            _            = False

-- Eq typeclass:
class Eq' a where       -- Eq in Prelude
  eq :: a -> a -> Bool  -- (==) in Prelude

instance (Eq' a, Eq' b) => Eq' (Pair a b) where
  eq = eqPair eq eq

instance (Eq' a, Eq' b) => Eq' (Either' a b) where
  eq = eqEither eq eq

instance Eq' a => Eq' (Maybe' a) where
  eq = eqMaybe eq

instance Eq' a => Eq' (List a) where
  eq = eqList eq

instance Eq' a => Eq' (BinTree a) where
  eq = eqBinTree eq

--------------------------------------------------------------------------------
-- Define functions (f1 .. f11) with the following signatures.

f1 :: (a, (b, (c, d))) -> (b, c)
f1 = undefined

f2 :: (a -> b) -> a -> b
f2 = undefined

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> (b -> a -> c)
f4 = undefined

f5 :: ((a, b) -> c) -> (a -> b -> c)
f5 = undefined

f6, f6', f6'' :: (a -> (b, c)) -> (a -> b, a -> c)

f6 g  = (\x -> fst (g x) , \x -> snd (g x))
f6' g = (fst . g , snd . g)

f6'' g = (h1, h2)
  where h1 x = fst (g x)
        h2 x = snd (g x)

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 = undefined

f8, f8' :: forall a b c. (Either a b -> c) -> (a -> c, b -> c)
f8' g = (g . Left, g . Right)
f8 g = (h1, h2)
  where h1 :: a -> c
        h1 x = g (Left x)

        h2 :: b -> c
        h2 x = g (Right x)

f9 :: (a -> c, b -> c) -> (Either a b -> c)
f9 = undefined

f10 :: Either (a, b) (a, c) -> (a, Either b c)
f10 = undefined

f11 :: (a, Either b c) -> Either (a, b) (a, c)
f11 = undefined

-- Bonus:
f12 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f12 = undefined

--