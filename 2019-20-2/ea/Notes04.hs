
{-# language DeriveGeneric, PatternSynonyms, DeriveFunctor, StandaloneDeriving,
    UndecidableInstances #-}

import GHC.Generics

-- stream:
-- https://stream.inf.elte.hu/livedash/fp04.mpd

-- Elemi funktorok + funktorok folyt.
------------------------------------------------------------

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- ADT-kel kapcs: sok típus megadása kis számú konstrukcióval

-- data One = One
-- data Zero
-- data Sum a b = Inl a | Inr b
-- data Prod a b = Prod a b

-- funktorok megadása "elemi" funktorokkal.


-- 1. identitás

-- id függvény: id x = x
newtype Id a = Id a          -- newtype Id a = a
                             -- type Id a = a

instance Functor Id where
  fmap f (Id a) = Id (f a)

type Id' a = a
-- instance Functor Id' where  -- nem működik!

-- 2. konstans funktor
-- const :: a -> b -> a
-- const x y = x

newtype Const a b = Const a

-- szintén egy konstans funktor:
-- newtype Foo a = Foo Int Int

instance Functor (Const a) where
  fmap f (Const a) = Const a

-- type Foo = Const (Int, Int)

-- 3. szorzat funktor
data Prod f g a = Prod (f a) (g a)
  deriving Show

-- példák Prod értékeire:

p1 :: Prod Maybe Maybe Int
p1 = Prod Nothing (Just 0)

p2 :: Prod Maybe [] Bool
p2 = Prod (Just True) [True, False]

fst' :: Prod f g a -> f a
fst' (Prod fa ga) = fa

snd' :: Prod f g a -> g a
snd' (Prod fa ga) = ga

instance (Functor f, Functor g) => Functor (Prod f g) where
  fmap f (Prod fa ga) = Prod (fmap f fa) (fmap f ga)
             -- f :: a -> b
             -- fa :: f a
             -- ga :: g a
             -- goal 1 :: f b
             -- goal 2 :: g b

-- példa:
-- fmap (+10) p1 == Prod Nothing (Just 10)
-- fmap not p2 == Prod (Just False) [False,True]

-- 4. összeg:
data Sum f g a = Inl (f a) | Inr (g a)
  deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (Inl fa) = Inl (fmap f fa)
  fmap f (Inr ga) = Inr (fmap f ga)

data Foo2 a = Foo2 a a

-- data Prod f g a = Prod (f a) (g a)
--   deriving Show
-- newtype Id a = Id a
-- newtype Const a b = Const a

-- Hogyan definiálhatók egyes funktorok Id, Const, Sum, Prod használatával?
type Foo2' a = Prod Id Id a

f1 :: Foo2' Int
f1 = Prod (Id 10) (Id 20)
  -- hole 1 :: Id Int
  -- hole 2 :: ...

f2 :: Foo2' Bool
f2 = Prod (Id True) (Id False)

data Foo3 a = Foo3A Int | Foo3B Int a

type Foo3' = Sum (Const Int) (Prod (Const Int) Id)

f3 :: Foo3' Int
f3 = Inl (Const 10)

f3' :: Foo3' Bool
f3' = Inr (Prod (Const 10) (Id True))

-- Mire jó ez?
-- Generikus programozás:
-- lásd: GHC.Generics

data MyData a = MD1 Int Int a | Md2 [Int]
  deriving Generic

-- csomó műveletet "generikusan" kapunk, pl szerializáció, pretty print,
-- gráfként való adatreprezentálás, etc.


-- Rekurzív adattípusnak mi a generikus reprezentációja?

-- data List a = Nil | Cons a (List a)
-- data List a = Nil | One a | Two a a | Three a a a ...
--      ... ω
--
-- List a = 0 + 1 + 2 + 3 + 4 .....
-- (rákeresni: sorösszegek típusként)

-- ezt fel tudjuk-e írni, Id, Const, Prod, Sum-al?
-- Nem tudjuk!

-- példa enum típus generikus reprezentációjára
data RGB = R | G | B
data Zero

-- véges enum generikus reprezentációja
type RGB'   = Either () (Either () ())
type RGB''  = Maybe (Maybe ())
type RGB''' = Maybe (Maybe (Maybe Zero))

-- nem tudunk végtelen nagy típuskifejezést írni ezért List a *nem*
-- reprezentálható az eddigi műveletekkel!

-- approximáció

-- data L0 a = Nil0   -- üres listák típusa
-- data L1 a =

type L0 a = ()               -- üres listák típusa
type L1 a = Maybe (a, L0 a)  -- max 1 elemű listák
type L2 a = Maybe (a, L1 a)  -- max 2 elemű listák
type L3 a = Maybe (a, L2 a)  -- ...

l0 :: L0 Int
l0 = ()

l1 :: L1 Int
l1 = Nothing

l1' :: L1 Int
l1' = Just (10, ())

l3 :: L3 Int
l3 = Just (10, (Just (20, (Just (0, ())))))

-- type L 0    a = ()
-- type L(n+1) a = Maybe (a, L n a)
-- type List a = L ω a

-- fixpontja annak a funktornak, hogy
-- data ListF a b = NilF | ConsF a b

-- lista bázis funktora
data ListF a b = NilF | ConsF a b
  deriving (Show, Functor)

type L0' a = ()
type L1' a = ListF a (L0' a)
type L3' a = ListF a (ListF a (ListF a ()))

-- type Lω a = ...

newtype Fix f = Fix (f (Fix f))
type List a = Fix (ListF a)

deriving instance Show (f (Fix f)) => Show (Fix f)

-- Fix f a = Fix (f (f (f (f (Fix f a)))))

-- lista: (ListF a) funktor tetszőleges sokszori
-- iterációja

pattern Nil = Fix NilF
pattern Cons a as = Fix (ConsF a as)

l0' :: List Int
l0' = Nil

l1'' :: List Int
l1'' = Cons 10 Nil

l2'' :: List Int
l2'' = Cons 10 (Cons 20 (Cons 20 Nil))

map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)

-- generikus függvény: foldr
--   listára: foldr függvény:
--   minden (véges) lista függvény foldr-el megírható

-- tetszőleges Fix f formájú típusra foldr általánosítható
-- (feltéve, hogy Functor f)

-- (ListF a b -> b)
-- data ListF a b = NilF | ConsF a b
-- (Either a b -> c) ~ (a -> c, b -> c)
-- (ListF a b -> b) ~ (() -> b, a -> b -> b)
--   ~ (b, a -> b -> b)

-- foldr :: (a -> b -> b) -> b -> ([a] -> b)
-- foldr :: (b, a -> b -> b) -> ([a] -> b)

foldr' :: (ListF a b -> b) -> ([a] -> b)
foldr' f []     = f NilF
foldr' f (a:as) = f (ConsF a (foldr' f as))

-- generikus fold: ha egy réteg konstruktor fel tudunk dolgozni,
-- akkor akárhány réteg konstruktor fel tudunk dolgozni.

--                      (1 réteg)     (összes réteg)
gfold :: (Functor f) => (f a -> a) -> (Fix f -> a)
gfold f (Fix fa) = f (fmap (gfold f) fa)
  -- fa :: f (Fix f)
  -- fmap (gfold f) fa :: f a

sumList :: List Int -> Int
sumList = gfold $ \x -> case x of
  NilF       -> 0
  ConsF a as -> a + as

data TreeF a b = LeafF a | NodeF b b
  deriving (Show, Functor)

type Tree a = Fix (TreeF a)

sumTree :: Tree Int -> Int
sumTree = gfold $ \t -> case t of
  LeafF n   -> n
  NodeF l r -> l + r

-- (lásd még: attribútum grammatika)
