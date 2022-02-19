{-# language EmptyDataDeriving, EmptyCase, KindSignatures, InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

-- ADT, Functor, kind
--------------------------------------------------------------------------------

-- |a|   :  lehetséges "a" értékek száma

-- |Zero| = 0
data Zero deriving Show

-- |One| = 1
data One = One

-- |Mul a b| = |a| * |b|      -- Descartes szorzat (pár típus, halmazelméletileg)
data Mul a b = Pair a b

-- |Add a b| = |a| + |b|            -- "Összeg típus": standard "Either a b"
data Add a b = Left' a | Right' b

-- példa:
t1 :: Add One One
t1 = Left' One

t2 :: Add One One
t2 = Right' One


-- (azonosságok csak akkor igazak, ha termináló, kivétel-mentes függvények vannak csak)
-- példa: loop-al lehet Zero-nak értéket adni:
zero :: Zero
zero = zero      -- zero rekurzívan a zero-ra hivatkozik,   (ones = 1 : ones)

-- aritmetikai azonosságok

-- 0 + a = a
-- |Add Zero a| = |a|

f1 :: Add Zero a -> a
f1 (Left' z)  = case z of          -- ez az ág nem lehetséges! (holt kód)
f1 (Right' a) = a

f1' :: a -> Add Zero a
f1' a = Right' a

zeroToAny :: Zero -> a
zeroToAny z = case z of    -- (holt kód, tetszőleges lehet a típusa)


-- 1 * a = a

f2 :: Mul One a -> a
f2 (Pair _ a) = a

f2' :: a -> Mul One a
f2' a = Pair One a

-- a * b = b * a         -- mezők felcserélése
-- a + b = b + a         -- bal/jobb konstruktor cseréje

f3 :: Add a b -> Add b a
f3 (Left' a)  = Right' a
f3 (Right' b) = Left' b

-- opcionális házi: oda-vissza függvények
-- a + (b + c) = (a + b) + c
-- a * (b * c) = (a * b) * c

-- disztributivitás:
-- a * (b + c) = (a * b) + (a * c)

f4 :: Mul a (Add b c) -> Add (Mul a b) (Mul a c)
f4 (Pair a (Left'  b)) = Left' (Pair a b)
f4 (Pair a (Right' c)) = Right' (Pair a c)

f4' :: Add (Mul a b) (Mul a c) -> Mul a (Add b c)
f4' (Left' (Pair a b))  = Pair a (Left' b)
f4' (Right' (Pair a c)) = Pair a (Right' c)

-- hatványozás: |Exp a b| = |a|^|b|

-- a^b = a * a * a * ..... a
--         b-szer szorozva

type Exp a b = b -> a

-- példa: párok

-- a^2 = a*a
-- |Exp a Bool| = a^2 = |Bool -> a|
data Square a = Square a a

f5 :: (Bool -> a) -> Square a
f5 f = Square (f True) (f False)

f5' :: Square a -> (Bool -> a)
f5' (Square a a') = \b -> if b then a else a'

-- 2^a : "hatványhalmaz"
-- a -> Bool

-- a^Nat           ("Nat": természetes számok típusa, Integer standard típus, ami lehet negatív is)
-- Nat -> a

data Stream a = Cons a (Stream a)  -- (lusta stream-ek típusa)

-- |Stream a| = |Nat -> a|

ones :: Stream Int
ones = Cons 1 ones

takeS :: Int -> Stream a -> [a]
takeS n str | n <= 0 = []
takeS n (Cons a as)  = a : takeS (n - 1) as


-- curry   :: ((a, b) -> c) -> (a -> (b -> c))
-- uncurry :: (a -> b -> c) -> ((a, b) -> c)

--         (c^b)^a = c^(a*b)
-- avagy : (a^b)^c = a^(b*c)


-- a^(b+c) = a^b * a^c
-- (Add b c -> a) -> Mul (b -> a) (c -> a)

f6 :: (Add b c -> a) -> Mul (b -> a) (c -> a)
f6 f = Pair (\b -> f (Left' b)) (\c -> f (Right' c))

f6' :: Mul (b -> a) (c -> a) -> (Add b c -> a)
f6' (Pair f g) (Left' b)  = f b
f6' (Pair f g) (Right' c) = g c

-- standard megfelelő:
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
--        :: Mul (a -> c) (b -> c) -> (Either a b -> c)


-- rekurzív típusoknál az egyszerű aritmetika már nem működik
-- data List a = Nil | Cons a (List a)
-- |List a| = 1 + |a| * |List a|
-- |List a| = 1 + |a| * (1 + |a| * (1 + |a| * |List a|))
--   átrendezzük, sorösszeg:
-- |List a| = 1 + a + a * a + a * a * a + a * a * a * a +... (választás az összes lehetséges hossz között!
-- |List a| = a^0 + a^1 + a^2 + a^3 ....


-- Funktorok
--------------------------------------------------------------------------------

-- standard osztály:

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Functor f : f-re van "map" függvény
-- map  :: (a -> b) -> [a] -> [b]                -- f = lista típus ([])
-- fmap :: (a -> b) -> Maybe a -> Maybe b        -- f = Maybe
-- fmap :: (a -> b) -> BinTree a -> BinTree b    -- f = BinTree

l1 :: [] Int    -- [Int] szintaktikus cukor
l1 = [0..10]

m1 :: Maybe Int
m1 = Just 0

-- data-val definiált paraméteres típus: n-paraméteres típuskonstruktort ad meg
--   - ne keverjük a különböző aritású típuskonstruktorokat: típusszinten is akarunk típusrendszert
--   - "kind" rendszer: típuskonstruktorok aritásai (típusok típusai)

-- Explicit kind annotáció:

class Functor' (f :: * -> *) where    -- * -> * : 1 paraméteres típus
  fmap' :: (a -> b) -> f a -> f b

-- "higher-kinded type" : paraméteres típusok fölötti absztrakció (HKT)
--   (C++ példa: template template)

-- * kind : 0-paraméteres (konkrét) típusok
-- Int  :: *
-- Bool :: *

-- ghci-ben
--  > :k <típuskifejezés>

-- * -> * : 1-paraméteres típusok

-- > :k Maybe
-- Maybe :: * -> *

-- > :k Maybe Int
-- Maybe Int :: *

-- > :k []
-- [] :: * -> *

-- > :k Either
-- Either :: * -> * -> *
-- > :k Either Int
-- Either Int :: * -> *

-- Either Int Int :: *

-- (* -> *) -> *
-- (* -> *) -> * -> *

data Foo f a = Foo (f a) (f a) deriving Show

foo1 :: Foo Maybe Int
foo1 = Foo (Just 10) (Just 20)

foo2 :: Foo [] Bool
foo2 = Foo [True, False] []


------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- fmap (+10) $ Node (Leaf 0) (Leaf 1) == Node (Leaf 10) (Leaf 11)

-- Either :: * -> * -> *
-- Either a :: * -> *         (utolsó típusparamétert tudjuk fmap-elni)

-- instance Functor (Either c) where
--   fmap :: (a -> b) -> (Either c) a -> (Either c) b
--   fmap f (Left c) = Left c
--   fmap f (Right a) = Right (f a)

-- fmap (+10) (Right 0) == Right 10
-- fmap not (Left 0) == Left 0

-- (,) :: * -> * -> *
-- (Int, Bool) :: *
-- (,) Int Bool :: *
-- (,) Int :: * -> *

-- instance Functor ((,) c) where
--    fmap :: (a -> b) -> (c, a) -> (c, b)
--    fmap f (c, a) = (c, f a)

-- fmap (+10) (True, 0) == (True, 10)


-- Kérdés: van-e olyan * -> * típus, ami nem Functor?

data NotFunctor a = NotFunctor (a -> Bool)     -- ha az "a" függvény *bemenetként* szerepel

  --  nincs ilyen típusú függvény (értelmesen):
  -- fmap :: (a -> b) -> NotFunctor a -> NotFunctor b

data Predicate a = Predicate (a -> Bool)

instance Functor Predicate where
  fmap :: (a -> b) -> Predicate a -> Predicate b
  fmap f (Predicate g) = Predicate (\b -> undefined)

     -- f :: a -> Bool
     -- b :: b

  -- köv előadás: "legális" / jól viselkedő Functor instance
  -- vesd össze: instance Eq a

  -- "variancia" : kovariáns / kontravariáns típusparaméterek

-- előre tudunk map-elni egy paraméter fölött: "kovariáns" paraméter
-- fordítva tudunk map-elni: "kontravariáns" paraméter

-- Functor: kovariáns map-elés osztálya

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant Predicate where
  contramap f (Predicate g) = Predicate (g . f)

data Invariant a = Invariant (a -> a) -- invariáns paraméter, kimenet is és bemenet is

--------------------------------------------------------------------------------
