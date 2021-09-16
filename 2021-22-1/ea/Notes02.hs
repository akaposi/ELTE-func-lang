
-- kérdés: mi a típus hatványozása?

-- "a" és "b" típus
-- Exp a b, amire igaz, hogy |Exp a b| = |a|^|b|

-- |b| másolatot tároljunk a értékből:
type Exp a b = b -> a

-- konkrét eset: t.f. |b| = 2  ,   b = Bool

-- négyzetre emelés?

data Square a = Square a a   --

-- pl. Square Bool  ~ (Bool, Bool)    |(Bool, Bool)| = 4 = 2^2

-- (a^b)^c = a^(b*c)   curryzés!

-- c -> (b -> a) = (b, c) -> a
-- c -> b -> a = (b, c) -> a
-- a -> b -> c = (a, b) -> c      -- curry/uncurry

-- extra házi: megnézni random azonosságokat hatványozásra

-- (a -> (b, c)) = (a -> b, a -> c)
-- (b * c)^a = b^a * c^a            -- stb..

-- rekurzív típusok: nem mindig működik

-- példa: Haskell típus, ami *nem* feleltethető meg halmaznak

data Foo = Foo (Foo -> Bool)

-- Foo ~ (Foo ->Bool)
-- Foo ~ 2^Foo          -- halmazokra sosem igaz

-- nem halmazokkal adjuk meg a szemantikát, hanem ún. "domain"-ekkel
-- (lásd: "denotációs szemantika")
-- (és akkor igaz lesz Foo ~ 2^Foo)

--------------------------------------------------------------------------------


-- típusosztályok
-- terv: Semigroup, Monoid
--       Functor

-- félév során: Functor > Applicative > Monad
--    összesen: 3 class, 4 metódus   (féléves anyag jelentős része)
{-

class Semigroup' a where -- "félcsoport"
  (<>) :: a -> a -> a
  -- elvárás: x <> (y <> z) = (x <> y) <> z     (minden instance-ra)

-- konvenció: class-ok: legyenek gyakori matematikai struktúrák
--                      legyen hozzájuk világos specifikáció

instance Semigroup' [a] where
  xs <> ys = xs ++ ys

instance Semigroup' Int where
  x <> y = x + y

-- választás több természetes instance között:

-- newtype: mint data, viszont csak 1 konstruktor és 1 mező
-- (futásidejű ktg nélküli wrapper)
newtype Add = Add Int
newtype Mul = Mul Int

instance Semigroup Add where
  Add x <> Add y = Add (x + y)

instance Semigroup Mul where
  Mul x <> Mul y = Mul (x * y)

------------------------------------------------------------

-- egységelemet adunk a <> művelethez
class Semigroup' a => Monoid' a where
  mempty :: a
  -- mempty <> x = x
  -- x <> mempty = x

instance Monoid' [a] where
  mempty = []
-}

-- függvény monoid:

newtype Fun a = Fun (a -> a)

instance Semigroup (Fun a) where
  Fun f <> Fun g = Fun (f . g)

instance Monoid (Fun a) where
  mempty = Fun id

-- (praktikus megj: ghci :i <név>)
--     típus, minek a metódusa, operátor kötés, hol van definiálva

-- extra megnézni:
-- instance Monoid b => Monoid (a -> b)

-- speciális eset: Monoid a => Monoid (a, a)

-- Functor
----------------------------------------------------------------------

-- standard
-- class Functor f where               -- f *nem* típus, hanem
--   fmap :: (a -> b) -> f a -> f b    -- típus operátor
                                       -- 1-paraméteres típus

--
--   1. törvény: fmap id x = x
--        -- map-elés nem változtat struktúrát

--   2. törvény: fmap f (fmap g x) = fmap (f . g) x
--        -- optimalizációs tulajdonság
--        -- két bejárás helyett elég egy is

-- Programozó által kontrollált GHC átírás
-- {-# REWRITE forall f g x. fmap f (fmap g x)  = fmap (f.g) x #-}

-- Functor: olyan struktúrák/típusok osztálya, amelyekre "map" függvény

-- data Maybe a = Nothing | Just a
-- Maybe Int -- konkrét típus
-- Maybe     -- 1-paraméteres típus operátor

{-
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just a) = Just (f a)
-}

-- fmap (+10) (Just 0) == Just 10
-- fmap (+10) Nothing  == Nothing

-- típusoperátorok rendszere (kind rendszer (típusok típusrendszere))
------------------------------------------------------------

-- instance Functor Int where

-- hiba: Expected kind ‘* -> *’, but ‘Int’ has kind ‘*’
-- Functor 1 paraméteres típust vár, viszont Int az egy konkrét típus

-- kind-ok:

-- * : konkrét típusok típusa
--    Int :: *
--    Bool :: *
--    stb.
-- ghci :k <típus>
-- >:k Bool
-- > Bool :: *

-- * -> * : 1-paraméteres típusok kind-ja
--  Maybe :: * -> *

-- * -> * -> * : 2-paraméteres
-- Either :: * -> * -> *
-- Either Int :: * -> *
-- Either Int Bool :: *

-- higher-kinded types (HKT):
--     bármi, ahol absztrahálunk paraméteres típusok fölött

-- példa: két funktor szorzata?

-- Product :: (* -> *) -> (* -> *) -> * -> *
data Product f g a = Product (f a) (g a)

p1 :: Product Maybe Maybe Int
p1 = Product (Just 30) Nothing

p2 :: Product Maybe [] Bool        -- :k [] :: * -> *
p2 = Product (Just True) [False, False]

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Product x y) = Product (fmap f x) (fmap f y)
     -- x :: f a
     -- y :: g a

-- elemi funktorok, + funktorok kombinálása

data Sum f g a = Inl (f a) | Inr (g a)

-- constant Functor                     (\x y -> x)
data Const a b = Const a

-- identity Functor                     (\x -> x)
newtype Id a = Id a

-- Funktor kompozíció                   (f . g) x = f (g x)
newtype Compose f g a = Compose (f (g a))

-- Mire jók ezek?
-- utána lehet nézni:
-- GHC.Generics modul,  "generic programming", "datatype-generic programming"

-- lényeg: nagyon sok típust felírunk kevés típus "művelet" segítségével
--   pl.  type Bool = Either () ()
--        type Three = Either () (Either () ())
--        ...

-- probléma: végtelen ("rekurzív") és paraméteres típusokkal
--           nem tudunk mit kezdeni

-- Funktorokal: tudjuk reprezentálni mindkettőt

-- példa: Lista

-- Funktorok fix pontja:

newtype Fix f = Fix (f (Fix f))
        -- Fix f ~ f (f (f ( f( f (........

-- Fix Maybe ~ Maybe (Maybe (Maybe .....

m1 :: Fix Maybe
m1 = Fix Nothing

m2 :: Fix Maybe
m2 = Fix (Just (Fix Nothing))
   -- Fix (Just (Fix (Just (Fix Nothing))))

-- "recurse" extra paraméter jelöli a rekurzió helyét
data ListF a recurse = Nil | Cons a recurse

-- List a ekvivalens a szokásos lista típussal
type List a = Fix (ListF a)

l1 :: List Int
l1 = Fix Nil

cons :: a -> List a -> List a
cons a as = Fix (Cons a as)

type ListF' a =  Sum (Const ()) (Product (Const a) Id)

-- Full generikus reprezentáció
type List' a = Fix (Sum (Const ()) (Product (Const a) Id))

------------------------------------------------------------
