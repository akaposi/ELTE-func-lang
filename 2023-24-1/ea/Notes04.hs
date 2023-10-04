{-# LANGUAGE KindSignatures, AllowAmbiguousTypes #-}

{-

:t [], :k []
:t (,,), :k (,,)

[1] :: [Int]
[1,2] :: [Int]

(1,"a") :: (Int,String)

(,),(->)

functor laws: fgv(co,contra),id,const,kompoz

Foldable
Monad
State

Applicative

ez az ora 4 perccel rovidebb

kov. orara: tuple miert max. 64?
            tombok megvalositasa
-}

f :: forall a . a -> a
f = \ x -> x
-- (Λ a . λ x . x)

-- :t []
-- [] :: forall a . [a]

-- * = tipusok tipusa, Agdaban * = Set = Type
-- Bool :: *, Int :: *, Bool -> Int :: *, ...

-- ha van egy Haskell program/term t :: A, akkor A :: *

-- ha A-nak vannak elemei, akkor A fajtaja (kind-ja) *

-- [] = Nil :: forall a . List a, ez egy term
-- [] = List :: * -> *, ez nem egy tipus, hanem egy tipusfuggveny
data List a = Nil | Cons a (List a)

xs :: [] Int -- itt [] egy tipusfuggveny, [] Int = [Int]
xs = [] -- ez egy term

-- Eq, Ord, Show

-- Eq a,  (==) :: a -> a -> Bool
-- minden x :: a-ra (x == x) = True
-- (x == y) = (y == x)
-- (x == y) = True, akkor (f x == f y) = True

-- x == y -> P(x) -> P(y)
-- True == False = True
-- (if True then (\ _ -> 0) else (\ _ -> 1))

class Finite (a :: *) where
  elemCount :: a -> Int  -- a egy dummy parameter
  elems     :: [a]
  -- length elems = elemCount

instance Finite Bool where
  elemCount _ = 2
  elems = [True,False]

instance Finite () where
  elemCount _ = 1
  elems = [()]

data Empty

instance Finite Empty where
  elemCount _ = 0
  elems = []

k :: Int
k = elemCount (undefined :: Bool)

{-
instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
  elemCount _ = elemCount (undefined :: b) ^ elemCount (undefined :: a)
  elems = undefined
  -- jo HF
-}

instance (Finite a, Finite b) => Finite (a,b) where
  elemCount _ = elemCount (undefined :: a) * elemCount (undefined :: b)
  elems = [ (a,b) | a <- elems, b <- elems ]

instance Finite a => Finite (Maybe a) where
  elemCount _ = elemCount (undefined :: a) + 1
  elems = Nothing : map Just elems

instance Finite (Bool -> Maybe Bool) where
  elemCount _ = 9
  elems =  --                         __________                              __________
    [ (\ b -> if b == elems !! 0 then elems !! 0 else if b == elems !! 1 then elems !! 0 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 0 else if b == elems !! 1 then elems !! 1 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 0 else if b == elems !! 1 then elems !! 2 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 1 else if b == elems !! 1 then elems !! 0 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 1 else if b == elems !! 1 then elems !! 1 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 1 else if b == elems !! 1 then elems !! 2 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 2 else if b == elems !! 1 then elems !! 0 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 2 else if b == elems !! 1 then elems !! 1 else undefined)
    , (\ b -> if b == elems !! 0 then elems !! 2 else if b == elems !! 1 then elems !! 2 else undefined)
    ]
{-
    [ (\ b -> if b then Nothing else Nothing)
    , (\ b -> if b then Nothing else Just False)
    , (\ b -> if b then Nothing else Just True)
    , (\ b -> if b then Just True else Nothing)
    , (\ b -> if b then Just True else Just False)
    , (\ b -> if b then Just True else Just True)
    , (\ b -> if b then Just False else Nothing)
    , (\ b -> if b then Just False else Just False)
    , (\ b -> if b then Just False else Just True)
    ]
-}

instance (Finite a, Eq b) => Eq (a -> b) where
  f == g =
    and [ f x == g x | x <- elems ]

g, h :: Maybe (Maybe (Maybe Bool)) -> Int
g _ = 4
h _ = 4

-- seemingly impossible functional programs
-- ilyen tipusu fuggvenyek egyenloseget eldonti: ((Int -> Bool) -> Bool)\

trues :: [Bool]
trues = True : trues

masik :: [Bool]
masik = take 100000000 trues ++ [False] ++ trues

class Functor' f where
  fmap :: (a -> b) -> f a -> f b
  -- fmap id = id
  -- fmap (f . g) = fmap f . fmap g

data Fura (a :: *) = A | B

{-
[], Maybe, (a,), (,a), Either a, "\b -> Either b a" , (->) a = "\ b -> (a -> b)"

"\ b -> (b -> a)"
-}



data Fun1 a b = Mk (a -> b) -- HF: instance Fun1 a
data Fun2 a b = Mk (b -> a) -- HF: nincs instance Fun2 a-ra

-- kov. ora 8 perccel rovidebb
