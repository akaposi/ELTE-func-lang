{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

module Gy04_pre where

import Control.Applicative hiding (ZipList(..))
import Prelude hiding (Maybe(..), Either(..))

-- Ami előző órán kimaradt:

-- Az fmap-nak van egy operátor verziója is:
-- <$>
-- tehát f <$> xs == fmap f xs

-- A Functor instance írási is algoritmizálható, van rá DeriveFunctor nyelvi kiegészítő.

data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Functor)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor)
data Either e a = Left e | Right a deriving (Eq, Show, Functor)
data Tuple e a = Tuple e a deriving (Eq, Show, Functor)
data Id a = Id a deriving (Eq, Show, Functor)
data Wrap f a = Wrap (f a) deriving (Eq, Show, Functor)
data Fun a b = Fun (a -> b) deriving Functor
data Compose f g a = Compose (f (g a)) deriving Functor
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor)


-- A Functor az szép és jó, viszont nem tud például több f-féle "konténert" kombinálni
-- Ilyen függvény pl nem létezik:
fmap2 :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap2 = error "Does not exist"

-- Ahhoz, hogy ilyet le tudjunk írni, erősebb megkötés kell => Applikatív
-- Ha a Functor volt az 1-és-sok érték (1 függvény és sok érték) kombinációja
-- az applikatív a sok-és-sok érték (sok függvény és sok érték) kombinációja

-- Applikatív törvények (nem kell tudni, de jó instance guidelineok):
{-
        Identitás:         pure (\x -> x) <*> v = v
        Kompozíció:        pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        Homomorfizmus:     pure f <*> pure x = pure (f x)
        Felcserélhetőség:  u <*> pure y = pure ($ y) <*> u
-}

-- Egy jó guideline:
-- Pl <*> esetén ha az f (a -> b) Q darab függvényt tartalmaz és az f a X darab a-t, akkor az f b Q * X darab b-t fog tartalmazni
-- Ez nem mindig igaz, de átlalában

(+++) :: List a -> List a -> List a
Nil +++ as = as
(Cons a as) +++ bs = Cons a (as +++ bs)

infixr 5 +++

instance Applicative List where

  pure :: a -> List a -- Tiszta értéket berak applikatív környezetbe
  pure = undefined

  liftA2 :: (a -> b -> c) -> List a -> List b -> List c -- Az előbb említett fmap2
  liftA2 = undefined

  (<*>) :: List (a -> b) -> List a -> List b -- Alternatív verziója a liftA2-nek, csak az egyiket kell megírni (liftA2 f a b = f <$> a <*> b, (<*>) = liftA2 ($))
  (<*>) = undefined

instance Applicative Tree where
  pure :: a -> Tree a
  pure = undefined
  liftA2 :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
  liftA2 = undefined
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) = undefined

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = undefined
  liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
  liftA2 = undefined
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) = undefined

-- Fixált paraméteres eseteknél a fix paraméter típusú elemeket nem tudjuk "kombinálni", ezért általában az elsőt kiválasztjuk
instance Applicative (Either e) where
  pure :: a -> Either e a
  pure = undefined
  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
  (<*>) = undefined
  liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
  liftA2 = undefined

instance Applicative Id where
  pure :: a -> Id a
  pure = undefined
  (<*>) :: Id (a -> b) -> Id a -> Id b
  (<*>) = undefined
  liftA2 :: (a -> b -> c) -> Id a -> Id b -> Id c
  liftA2 = undefined

instance Applicative f => Applicative (Wrap f) where
  pure :: Applicative f => a -> Wrap f a
  pure = undefined
  liftA2 :: Applicative f => (a -> b -> c) -> Wrap f a -> Wrap f b -> Wrap f c
  liftA2 = undefined
  (<*>) :: Applicative f => Wrap f (a -> b) -> Wrap f a -> Wrap f b
  (<*>) = undefined

instance Applicative (Fun q) where
  pure :: a -> Fun q a
  pure = undefined
  liftA2 :: (a -> b -> c) -> Fun q a -> Fun q b -> Fun q c
  liftA2 = undefined
  (<*>) :: Fun q (a -> b) -> Fun q a -> Fun q b
  (<*>) = undefined

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: (Applicative f, Applicative g) => a -> Compose f g a
  pure = undefined
  liftA2 :: (Applicative f, Applicative g) => (a -> b -> c) -> Compose f g a -> Compose f g b -> Compose f g c
  liftA2 = undefined
  (<*>) :: (Applicative f, Applicative g) => Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (<*>) = undefined

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = undefined
  liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
  liftA2 = undefined
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) = undefined


-- Egy típusnak több szabályos applikatív instance-a is lehet
data ZipList a = ZipList [a] deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure = undefined
  liftA2 :: (a -> b -> c) -> ZipList a -> ZipList b -> ZipList c
  liftA2 = undefined
  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (<*>) = undefined

-- Az Applikatívot "statikus mellékhatás"-nak is szokás hívni, mivel az eredmény struktúrája eldönthető a paraméterek megvizsgálásával
-- Ez annyi megkötést ad nekünk, hogy a konténer struktúrája nem függ valami függvény eredményétől!
-- Jövő heti +/- egy applikatív instance írása

-- Gyakorlás: Írj az alábbi típusokra Applicative instance-ot!

data Product f g a = Product (f a) (g a) deriving (Eq, Show, Functor)
data Sum f g a = SumL (f a) | SumR (g a) deriving (Eq, Show, Functor)
data Kleisli f a b = Kleisli (a -> f b) deriving Functor
data BiRecurse a = LeftRec a (BiRecurse a) | RightRec a (BiRecurse a) | BNill deriving (Eq, Show, Functor)
