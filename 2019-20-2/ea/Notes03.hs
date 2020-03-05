{-# LANGUAGE KindSignatures #-}

import Prelude
  hiding (Semigroup(..), Monoid(..), Functor(..), Applicative(..),
          Monad(..))

-- Semigroup, Monoid, Functor
------------------------------------------------------------

-- típusosztályok használata

-- a típushoz adjunk meg egy "default" értéket
class Default a where
  default' :: a

instance Default Int where
  default' = 15

-- baj: nincs "természetes" specifikáció, arra, hogy
-- mi legyen a default érték

-- Haskell-ben gyakorlat:
--  nem túl sok osztály, viszont amit használunk,
--  az legyen "matematikailag" megalapozott

-- általában két inspiráció:
--      1. algebra
--      2. kategóriaelmélet

-- félcsoport: egy asszociatív bináris művelet
class Semigroup (a :: *) where
  (<>) :: a -> a -> a
  infixr 6 <>
  -- <> asszociatív legyen!
  --   (a <> b) <> c = a <> (b <> c)

instance Semigroup [a] where
  (<>) = (++)

-- nem lehet duplikált instance, viszont Int-re két természetes
-- instance is meg lenne adható:

-- instance Semigroup Int where
--   (<>) = (+)

-- instance Semigroup Int where
--   (<>) = (*)

-- megoldás: newtype wrapper típusra definiáljuk az instance-ot
-- newtype: pontosan egy konstruktor és egy mező
-- newtype: nincs runtime reprezentáció

-- bevezetünk két wrapper típust, az instance-ok
-- különböznek, mint Int esetén

newtype Add  = Add Int   deriving Show
newtype Mult = Mult Int  deriving Show

instance Semigroup Add where
  Add x <> Add y = Add (x + y)

instance Semigroup Mult where
  Mult x <> Mult y = Mult (x * y)


-- egységelemes félcsoport
class Semigroup a => Monoid (a :: *) where
  mempty :: a
  -- mempty legyen egységeleme a (<>)-nak
  -- mempty <> a = a
  -- a <> mempty = a

instance Monoid [a] where
  mempty = []

instance Monoid Add where
  mempty = Add 0

instance Monoid Mult where
  mempty = Mult 1

instance Semigroup a => Semigroup (Maybe a) where
  Just x  <> Just y  = Just (x <> y)
  Nothing <> Just y  = Just y
  Just x  <> Nothing = Just x
  Nothing <> Nothing = Nothing

-- példa: Just (Add 0) <> Just (Add 10) <> Just (Add 20) <> Nothing
--        == Just (Add 30)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  -- (<>) :: (a, b) -> (a, b) -> (a, b)
  (a, b) <> (a', b') = (a <> a', b <> b')
  -- példa: ("foo", "bar") <> ("a", "b") == ("fooa","barb")

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  -- ("foo", "bar") <> mempty == ("foo", "bar")

-- field accessor
-- ----------------------------------------

data Foo = Foo {fstFoo :: Int, sndFoo :: Int}
  deriving Show

-- fstFoo :: Foo -> Int   -- kiszedi az egyik Int-et
-- sndFoo :: Foo -> Int   -- kiszedi a másik Int-et

-- fstFoo (Foo 10 20) == 10
-- sndFoo (Foo 10 20) == 20

-- akkor érdemes használni, ha csak egy konstruktor van

-- ----------------------------------------

--- std. lib: Data.Monoid: Endo
newtype Fun a = Fun {unFun :: a -> a}
-- (a -> a) "endofüggvény"  (domain és codomain ugyanaz)

-- unFun :: Fun a -> (a -> a)
-- Fun   :: (a -> a) -> Fun a

instance Semigroup (Fun a) where
  -- (<>) :: Fun a -> Fun a -> Fun a
  Fun f <> Fun g = Fun (f . g)

instance Monoid (Fun a) where
  mempty = Fun id

-- "generikus" függvény tetszőleges Monoid a-ra
--        constraint => ...
-- több constraint:  (c1, c2, c3) => ....
mconcat :: Monoid a => [a] -> a
mconcat = foldr (<>) mempty
-- mconcat []     = mempty
-- mconcat (a:as) = a <> mconcat as

-- mconcat [Just "foo", Just "bar", Nothing, Nothing]
-- == Just "foobar"

-- unFun (mconcat [Fun (+10), Fun (+30)]) 20
-- == 60

-- Functor
--------------------------------------------------

-- félév során:
-- 3 darab osztály az anyag jelentős része:
--  Functor, Applicative, Monad
--  összesen van 4 darab metódus

-- "map-elhető típusok"
-- figyelem: f nem egy konkrét típus, hanem
-- egy 1-paraméteres típuskonstruktor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  -- + specifikáció:
  -- fmap id fa = fa       -- identitás fv-el map-elés ne csináljon
  --                       -- semmit (röviden: fmap id = id)
  -- fmap (f . g) fa = fmap f (fmap g fa)
  --                       -- röviden (fmap (f . g) = fmap f . fmap g)

  --   pl: map (+10) (map (+20) [0..10])
  --       = map (+30) [0..10]
  -- (első szabály: struktúra nem módosul)
  -- (második: optimalizációs szabály)

-- pl. Functor Maybe
--   fmap :: (a -> b) -> Maybe a -> Maybe b
--       Functor []
--   fmap :: (a -> b) -> [a] -> [b]

list :: [] Int
list = [0, 1, 2]

m :: Maybe Int
m = Nothing

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative f => Monad f where
--   (>>=) :: f a -> (a -> f b) -> f b

instance Functor [] where
  -- fmap :: (a -> b) -> [] a -> [] b
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)
  -- Maybe-re érdemes úgy gondolni, mint 0 vagy 1 elemű lista


-- kitekintés: típusoperátorok
--------------------------------------------------------------------------------

-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#kinds-and-some-type-foo

-- típusok + típusoperátorok nyelve
--   típusok típusa: *
--   ghci-ben
--  > :k <típuskifejezés>
--  megmondja a típuskifejezés típusát

-- :k Int
-- Int :: *         "Int egy típus"

-- :k Maybe
-- Maybe :: * -> *  "Maybe egy 1-paraméteres típuskonstruktor"

-- :k []
-- [] :: * -> *

-- :k Either
-- Either :: * -> * -> *

-- (parciális applikáció)
-- :k Either Int
-- Either Int :: * -> *

-- van-e olyan típus, hogy (* -> *) -> * -> *

data ParamList f a = Nil | Cons (f a) (ParamList f a)
  deriving Show

l1 :: ParamList Maybe Int
l1 = Cons (Just 10) $ Cons (Just 20) Nil

-- :k Functor
-- Functor :: (* -> *) -> Constraint

-- :k Eq
-- Eq :: * -> Constraint

--  constraint!
-- Int => a -> a -> Bool    -- rosszul típusozott, mivel
--                          -- => bal oldalán Constraint kell!
f :: Eq a => a -> a -> Bool
f = undefined


instance Functor (Either c) where
  -- f ebben az esetben (Either c)
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Either c a -> Either c b
  fmap f (Left c)  = Left c
  fmap f (Right b) = Right (f b)

  -- map-elés a második típusparaméter fölött történik!
  -- (Right konstruktor map-eljük)

-- (,) :: * -> * -> *
-- instance Functor ((,) a) where
--   fmap f (x, y) = (x, f y)


-- Általánosságban mi Functor?
-- minden Functor, ami valami a-típusú értéket tárol

-- Példa: milyen (f :: * -> *) létezik, ami *nem* Functor?

newtype NotFunctor a = NotFunctor (a -> Bool)

-- nem működik
-- instance Functor NotFunctor where
  -- fmap :: (a -> b) -> NotFunctor a -> NotFunctor b
  -- fmap f (NotFunctor g) = NotFunctor (\b -> _) --
                          --             b -> Bool
