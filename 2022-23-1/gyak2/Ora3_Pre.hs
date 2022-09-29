{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
module Ora3 where

import Prelude hiding (NonEmpty(..))

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show, Foldable)
data List a = Cons' a (List a) | Nil deriving (Eq, Show, Foldable)

-- Mire jó a foldable?
-- :i Foldable <- megmutatja milyen műveleteket kapunk ingyen csak a foldr segítségével

-- pl.:

sumFold :: (Foldable f, Num a) => f a -> a -- nem tudunk most mintailleszteni, kénytelenek vagyunk foldr-t használni
sumFold = undefined

elemFold :: (Foldable f, Eq a) => a -> f a -> Bool
elemFold = undefined

-- {-# MINIMAL ... #-} pragma -> minimális szükséges definíció egy típusosztály instancehoz
-- Eq esetén (==) vagy (/=), Foldable esetén foldr vagy foldMap
-- foldMap :: Monoid m => (a -> m) -> t a -> m

-- Semigroup egységelemes félcsoport (van valami asszociatív művelete)
-- pl lista:

-- szabály: (a <> b) <> c == a <> (b <> c)
instance Semigroup (List a) where
    (<>) = undefined

-- szabály: mempty <> a == a <> mempty == a
instance Monoid (List a) where
    mempty = undefined

-- Számok is félcsoportok, de azokra több definíció is lehet pl
-- (<>) = (+), mempty = 0
-- (<>) = (*), mempty = 1

-- Ezek segítségével írjunk Foldable-t
data List_ a = Cons_ a (List_ a) | Nil_

instance Foldable List_ where
    foldMap = undefined


-- Foldable gyengesége: Csak lebontani tud típusokat
-- map :: Foldable f => (a -> b) -> f a -> f b <-- ilyen nincs

-- Pedig ilyet lehet írni
mapList :: (a -> b) -> List a -> List b
mapList = undefined


data One a = One a
data Two a = Two a a

mapOne :: (a -> b) -> One a -> One b
mapOne = undefined

mapTwo :: (a -> b) -> Two a -> Two b
mapTwo = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

mapEither :: (a -> b) -> Either e a -> Either e b -- második paraméteren mappolunk
mapEither = undefined

mapTree :: (a -> b) -> Tree a -> Tree b -- Mintaillesztés!!!
mapTree = undefined

-- Ennek a funkciónak is van típusosztálya. A Functor
{-
:i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
-}

instance Functor List where
    fmap = undefined

instance Functor Tree where
    fmap = undefined

-- Mik járnak a Functor-ral?
-- Fmap operátor formában (<$>)

mapFunctor :: Functor f => (a -> b) -> f a -> f b
mapFunctor f d = f <$> d

-- Egy "put in" operátor (<$)

putIn :: Functor f => b -> f a -> f b
putIn = undefined

data Seven a = Seven a a a a a a a
data NonEmpty_ a = Last a | ConsNE a (NonEmpty_ a)
data SkipList a = ConsSL a (SkipList a) | Skip (SkipList a) | NilSL

-- Írjunk a fenti típusokra Functor instance-ot

instance Functor Seven where
    fmap = undefined

instance Functor NonEmpty_ where
    fmap = undefined

instance Functor SkipList where
    fmap = undefined
