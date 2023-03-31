{-# LANGUAGE InstanceSigs, PolyKinds, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Konzi where

-- import Prelude hiding (Maybe(..))
import Control.Applicative (Applicative(..))
import Data.Functor (Functor(..))
-- import GHC.Base (Eq(..))
import GHC.Show (Show(..))


    {- Típus konstruktor-}  {- Típus paraméter -}         {- Adatkonstruktorok-}
data Maybe                       a =                       Just a | Nothing

-- Ha van típus paraméter akkor annyi darab * van benne
-- :k Maybe -- * -> *

-- :seti -XNoStarIsType --> Type -> Type

-- A funktornak több "képességet ad"
-- Funktor nem tudott több dolgot kombinálni
-- fmap :: (a -> b) -> f a -> f b
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c

-- "statikus mellékhatás"
-- f <$> a <*> b <*> c <*> d ...
--- liftAK f a b c d ...
               -- Nil
data NonEmpty a = Last a | Cons a (NonEmpty a) deriving (Show, Functor)

-- data Wrap f a = Wrap (f a)

(+++) :: NonEmpty a -> NonEmpty a -> NonEmpty a
Last a +++ xs = Cons a xs
Cons a as +++ xs = Cons a (as +++ xs)

-- permutáció-féle = pure 1 elemet ad vissza
-- zipWith féle = pure végtelen elemet ad vissza
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure = Last

  liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
  liftA2 f (Last a) bs = fmap (\b -> f a b) bs -- amikor 1 db elem van bal oldalt és K darab elem jobb oldalt => 1 * K = K
  liftA2 f (Cons a as) bs = fmap (\b -> f a b) bs +++ liftA2 f as bs-- egy elem esetén fmap -> aztán konkatenáció egy rekurzív hívással

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (Last f) <*> as = fmap f as
  (Cons f fs) <*> as = fmap f as +++ (fs <*> as)

data Lift f a = Lift (f a)

-- PolyKinds (Polymorphic Kinds)
-- :seti -XPolyKinds

data Fix f a = Fix (f (Fix f a))

weirdfix :: Fix Maybe Functor
weirdfix = Fix Nothing

-- weirdfix' :: Fix Maybe (forall a. a -> a)
-- weirdfix' = Fix Nothing
