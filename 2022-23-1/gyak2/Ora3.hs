{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}
module Ora3 where

import Prelude hiding (NonEmpty(..))

-- Előző kisfeladatba megírtuk az alábbi típust:
data NonEmpty a = Last a | Cons a (NonEmpty a)
infixr 5 `Cons`
 
-- Írjunk a típusra Foldable instance-ot! Deriving semmilyen formában nem használható. Maximum pontért egy totális foldr implementáció szükséges! (2 pont)
instance Foldable NonEmpty where
    foldr f b (Last a) = f a b
    foldr f b (Cons a ne) = f a $ foldr f b ne

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show, Foldable)
data List a = Cons' a (List a) | Nil deriving (Eq, Show, Foldable)

-- Mire jó a foldable?
-- :i Foldable <- megmutatja milyen műveleteket kapunk ingyen csak a foldr segítségével

-- pl.:

sumFold :: (Foldable f, Num a) => f a -> a -- nem tudunk most mintailleszteni, kénytelenek vagyunk foldr-t használni
sumFold = foldr (+) 0

elemFold :: (Foldable f, Eq a) => a -> f a -> Bool
elemFold e l = foldr (\x p -> p || x == e) False l 

-- {-# MINIMAL ... #-} pragma -> minimális szükséges definíció egy típusosztály instancehoz
-- Eq esetén (==) vagy (/=), Foldable esetén foldr vagy foldMap
-- foldMap :: Monoid m => (a -> m) -> t a -> m

-- Semigroup egységelemes félcsoport (van valami asszociatív művelete)
-- pl lista:

-- szabály: (a <> b) <> c == a <> (b <> c)
instance Semigroup (List a) where
    Nil <> xs = xs
    (Cons' x xs) <> ys = Cons' x (xs <> ys)

-- szabály: mempty <> a == a <> mempty == a
instance Monoid (List a) where
    mempty = Nil

-- Számok is félcsoportok, de azokra több definíció is lehet pl
-- (<>) = (+), mempty = 0
-- (<>) = (*), mempty = 1

-- Ezek segítségével írjunk Foldable-t
data List_ a = Cons_ a (List_ a) | Nil_

instance Foldable List_ where
    foldMap f Nil_ = mempty
    foldMap f (Cons_ a as) = f a <> foldMap f as


-- Foldable gyengesége: Csak lebontani tud típusokat
-- map :: Foldable f => (a -> b) -> f a -> f b <-- ilyen nincs

-- Pedig ilyet lehet írni
mapList :: (a -> b) -> List a -> List b
mapList f Nil = Nil
mapList f (Cons' a l) = Cons' (f a) $ mapList f l


data One a = One a
data Two a = Two a a

mapOne :: (a -> b) -> One a -> One b
mapOne f (One p) = One $ f p

mapTwo :: (a -> b) -> Two a -> Two b
mapTwo f (Two a b) = Two (f a) (f b)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b -- data Maybe a = Just a | Nothing
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

-- data Either e a = Left e | Right a
mapEither :: (a -> b) -> Either e a -> Either e b -- második paraméteren mappolunk
mapEither f (Left e) = Left e
mapEither f (Right a) = Right $ f a

mapTree :: (a -> b) -> Tree a -> Tree b -- Mintaillesztés!!!
mapTree f (Leaf a) = Leaf $ f a
mapTree f (Branch left right) = Branch (mapTree f left) (mapTree f right)

-- Ennek a funkciónak is van típusosztálya. A Functor
{-
:i Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
-}

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons' a as) = Cons' (f a) (fmap f as)

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Branch left right) = Branch (f <$> left) (f <$> right)

-- Mik járnak a Functor-ral?
-- Fmap operátor formában (<$>)

mapFunctor :: Functor f => (a -> b) -> f a -> f b
mapFunctor f d = f <$> d

-- Egy "put in" operátor (<$)

putIn :: Functor f => b -> f a -> f b
putIn b fa = fmap (const b) fa

data Seven a = Seven a a a a a a a
data NonEmpty_ a = LastNE a | ConsNE a (NonEmpty_ a)
data SkipList a = ConsSL a (SkipList a) | Skip (SkipList a) | NilSL

-- Írjunk a fenti típusokra Functor instance-ot

instance Functor Seven where
    fmap f (Seven a b c d e f' g) = Seven (f a) (f b) (f c) (f d) (f e) (f f') (f g)

instance Functor NonEmpty_ where
    fmap f (LastNE a) = LastNE $ f a
    fmap f (ConsNE a as) = ConsNE (f a) $ fmap f as

instance Functor SkipList where
    fmap f (ConsSL a as) = ConsSL (f a) $ fmap f as
    fmap f (Skip as) = Skip $ fmap f as
    fmap f NilSL = NilSL
