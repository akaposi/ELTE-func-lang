{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}

module Ora5 where

import Prelude hiding (Maybe(..), Either(..))
import Control.Applicative

data List a     = Cons a (List a) | Nil deriving (Show, Functor)
data One a      = One a                 deriving (Show, Functor)
data Maybe a    = Just a | Nothing      deriving (Show, Functor)
data Either e a = Left e | Right a      deriving (Show, Functor)

-- Hogyan is nézett ki az fmap?
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- Mi a következő logikai lépés innen?
{-
    Több paraméter:
        liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
        fmap   :: Functor     f => (a      -> b) -> f a        -> f b
    A függvény is be van csomagolva
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b
        fmap  :: Functor     f =>   (a -> b) -> f a -> f b
    Mellékhatás avagy a függvény eredménye lehet mellékhatásos (ez volt a Monad)
        flip (>>=) :: Monad   f => (a -> f b) -> f a -> f b
        fmap       :: Functor f => (a ->   b) -> f a -> f b

    Akit érdekel bónusz (nem tananyag): Monád fordítottja
        extend :: Comonad f => (f a -> b) -> f a -> f b
        fmap   :: Functor f => (a   -> b) -> f a -> f b
-}

{-
:i Applicative
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
-}

-- Vannak törvények amiket a műveleteknek be kell tartani, de ezek nem érdekesek:
{-
Identitás:
    pure id <*> v ≡ v
Kompozíció:
    pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
Homomorfizmus:
    pure f <*> pure x ≡ pure (f x)
Felcserélhetőség:
    u <*> pure y ≡ pure ($ y) <*> u
-}

-- Minimális szükséges definíció: pure és ((<*>) vagy liftA2)
-- pure ugyanaz mint a return

instance Applicative One where
    pure :: a -> One a
    pure = undefined
    (<*>) :: One (a -> b) -> One a -> One b
    (<*>) = undefined
    liftA2 :: (a -> b -> c) -> One a -> One b -> One c
    liftA2 = undefined
    -- gyakorlás kedvéért mindet megírjuk

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = undefined
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) = undefined
    liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
    liftA2 = undefined

instance Applicative (Either e) where -- hibakomponens típusa fix
    pure :: a -> Either e a
    pure = undefined
    (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (<*>) = undefined
    liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
    liftA2 = undefined

instance Applicative List where
    pure :: a -> List a
    pure = undefined
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) = undefined
    liftA2 :: (a -> b -> c) -> List a -> List b -> List c
    liftA2 = undefined

-- Applikatív instance nem egyértelmű, nincs rá kódgenerátor.
data List' a = Cons' a (List' a) | Nil' a deriving (Functor, Show)

instance Applicative List' where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

-- Mi jár pluszba az applikatívval?
-- (*>) és (<*). Az egyik elemet eldobja, de a mellékhatását alkalmazza

-- egyéb lift műveletek pl liftA3, liftA4 stb

liftA3' :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3' f fa fb fc = undefined

liftA4' :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4' f fa fb fc fd = undefined

-- Applikatív folytatása a Monád
{-
:i Monad
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a -> m a
    {-# MINIMAL (>>=) #-}
-}

-- return-t meg se kell írni, alap defníciója return = pure


-- Amit a monád tud de az applikatív nem: Belső érték kihat a külső struktúrára, applikatívnál ilyet nem lehet
-- Monádnak is vannak törvényeik:
{-
Bal identitás:
    return a >>= k ≡ k a
Jobb identitás:
    m >>= returk ≡ m
Asszociativitás:
    m >>= (\x -> k x >>= k) ≡ (m >>= k) >>= h
-}

instance Monad One where
    (>>=) :: One a -> (a -> One b) -> One b
    (>>=) = undefined

instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) = undefined

instance Monad (Either e) where
    (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (>>=) = undefined

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) = undefined

-- Egyéb monád műveletek:
-- Kompozíció:
{-
    flip (.) ::            (a ->   b) -> (b -> c)   -> a ->   c
    (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-}

-- Megtalálható a Control.Monad könyvtárban
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = undefined

-- Join művelet
join :: Monad m => m (m a) -> m a
join mma = undefined