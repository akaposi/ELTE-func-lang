{-# language KindSignatures, InstanceSigs #-}


-- Algebrai adattípus

-- a * b, a + b, a ^ b, 0, 1
-- a - b, a / b, log f      data Double a = Double a a
--                          log Double = Bool

-- data List a = Nil | Cons a (List a)

-- List a = 1 + a * List a
-- List a = 1 + a * (1 + a * List a)
-- List a = 1 + a * (1 + a * (1 + a * List a) ...
-- List a = 1 + a + a^2 + a^3 + a^4 ...

-- Deriválás:
--    f   1-paraméteres típus
--    df  1-paraméteres típus    (Zipper struktúra)

-- lista deriváltja:
--     data Zipper a = Zipper [a] a [a]
--                     Zipper [a] Int
--        [_, _, _, _*, _, _, _]
--     lista amiben "fókuszálunk" egy pozícióra

-- általánosan: deriváltja egy 1-paraméteres típusnak:
--               ráfókuszál egy értékre, O(1) időben az érték elérhető
--               O(1) időben egy lépést lehet bármilyen irányba
--               tenni

-- sqrt b =
-- (sqrt b * sqrt b) ~ b

-- "Algebrai" másik jelentése (matematikai)
--     "Algebrai struktúra"

-- a halmaz    (típusparaméter)

-- Listₐ halmaz
-- nil  ∈ Listₐ
-- cons ∈ (a × Listₐ → Listₐ)

-- Kind-ok, Functor osztály
------------------------------------------------------------

-- foo :: Maybe
-- foo = 100

-- kind: típus-szintű dolgok típusa

-- * : típusok kind-ja
-- > :k Int
-- (ha "a :: *", akkor "a" egy típus)
-- > :k Maybe
-- Maybe :: * -> *

-- * -> * : 1 paraméteres típusok kind-ja

-- Maybe Int :: *

-- f :: a -> b
-- f a :: b

-- Either :: * -> * -> *
-- Either Int :: * -> *
-- Either Int Bool :: *

-- Beépített típusok: [a], (a, b), (a, b, c), (), a -> b
-- Ezek mind szintaktikus cukrosak
-- Mindnek van prefix (cukor nélküli formája)

-- Lista:
-- > :k []
-- [] :: * -> *

l1 :: [] Int
l1 = [0, 1, 2]

-- Tuple:
-- > :k (,)
-- (,) :: * -> * -> *
-- > :k (,,)
-- (,,) :: * -> * -> * -> *

-- :k (,,) Bool
-- (,,) Bool :: * -> * -> *

-- :k ()
-- () :: *

-- 1 * a * b * c * ... * z
-- 0 + a + b + c + ... + z

-- 0 méretű tuple : 1 elemű típus

--   :: (* -> *) -> *
--

-- {-# language KindSignatures #-}
data Foo (f :: * -> *) = Foo (f Int)

foo1 :: Foo Maybe
foo1 = Foo (Just 100)

foo2 :: Foo []
foo2 = Foo [0, 1, 2]

-- érték szinten:
--  f :: ((Bool -> Bool) -> Bool) -> Bool
-- típus szinten:
--  data Foo
--  Foo :: ((* -> *) -> *) -> *

-- Functor
------------------------------------------------------------

-- map :: (a -> b) -> [a] -> [b]

-- Functor osztály: map függvény a metódusa

-- osztály paramétere 1 paraméteres típus
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   fmap :: (a -> b) -> [] a -> [] b
--   fmap = map

-- instance Functor Maybe where
--   fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap f Nothing  = Nothing
--   fmap f (Just a) = Just (f a)

-- Általánosan:
--   fmap :: (a -> b) -> f a -> f b
---     "f" valamilyen struktúra, fmap alkalmaz egy függvényt
--      minden tárolt értékre

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- Több mint egy paraméteres típus:
--   alkalmazzuk a típus N-1 paraméterre
--   marad egy 1 paraméteres típus
--   (utolsó paraméter fölött map-elünk)
--
-- pl: Either második parmétere fölött map-elünk
--
-- instance Functor (Either c) where
--   fmap :: (a -> b) -> Either c a -> Either c b
--   fmap f (Left c)  = Left c
--   fmap f (Right a) = Right (f a)

-- flip f x y = f y x

newtype Flip f a b = Flip {unFlip :: f b a}
-- unFlip :: mező projekció
-- unFlip :: Flip f a b -> f b a    (Flip konstruktor inverze)

instance Functor (Flip Either c) where
  fmap :: (a -> b) -> Flip Either c a -> Flip Either c b
  fmap f (Flip (Left a)) = Flip (Left (f a))
  fmap f (Flip (Right c)) = Flip (Right c)

-- class Bifunctor f where
--   bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
--   lmap  :: (a -> b) -> f a c -> f b c
--   rmap  :: (a -> b) -> f c a -> f c b

-- class Multifunctor (f :: (k -> *) -> *) where

-- instance Functor ((->) c) where
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)
--   fmap f g = \c -> f (g c)

-- c -> a     annyi darab "a" értéket tárol, ahány
--            "c" érték van
--

-- Kérdés: mi egy olyan "f :: * -> *" amire nincs
-- Functor instance?

newtype Predicate a = Predicate (a -> Bool)

data Bad a = Bad (a -> Bool)

instance Functor Bad where
  fmap :: (a -> b) -> Bad a -> Bad b
  fmap f (Bad g) = Bad (\a -> True)
    -- f :: a -> b
    -- g :: a -> Bool

-- ha a paraméter függvény bemenetként megjelenik
-- akkor nincs Functor instance

-- class Contravariant f where
--   contramap :: (a -> b) -> f b -> f a

-- sima "Functor" osztály: "kovariáns funktor"
-- "Contravariant" osztály: "kontravariáns funktor"

------------------------------------------------------------
-- Osztály törvények: konvenciók, aminek érdemes megfelelni

-- Funcktor törvények:
--   1.   fmap id x = x
--   2.   fmap f (fmap g x) = fmap (f . g) x

--  1. fmap-elés csak az értékeket változtatja, a struktúrát nem
--  2. "hatékonysági" konvenció: elég csak egy bejárás
