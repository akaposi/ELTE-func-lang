{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving #-}
module Gy06 where

import Prelude hiding (Maybe(..), Either(..))

-- Definiáljuk egy függvényt, amely egy "mellékhatásos" függvényt végig mappol egy listán
--                                          v az m mellékhatást összegyűjtjük
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f [] = return []
mapMList f (a : as) = do
  ma <- f a
  mas <- mapMList f as
  return $ ma : mas

-- Mivel a Functor (sima mappolás) általánosítható volt, ez a mellékhatásos mappolás is lehet általánosítható
data Single a = Single a deriving (Eq, Show, Functor, Foldable)
data Tuple a = Tuple a a deriving (Eq, Show, Functor, Foldable)
data Quintuple a = Quintuple a a a a a deriving (Eq, Show, Functor, Foldable)
data List a = Nil | Cons a (List a) deriving (Eq, Show, Functor, Foldable)
data Maybe a = Just a | Nothing deriving (Eq, Show, Functor, Foldable)
data NonEmpty a = Last a | NECons a (NonEmpty a) deriving (Eq, Show, Functor, Foldable)
data NonEmpty2 a = NECons2 a (List a) deriving (Eq, Show, Functor, Foldable)
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show, Functor, Foldable)
data Either e a = Left e | Right a deriving (Eq, Show, Functor, Foldable)
data BiTuple e a = BiTuple e a deriving (Eq, Show, Functor, Foldable)
data TriEither e1 e2 a = LeftT e1 | MiddleT e2 | RightT a deriving (Eq, Show, Functor, Foldable)
data BiList a b = ACons a (BiList a b) | BCons b (BiList a b) | ABNill deriving (Eq, Show, Functor, Foldable)
data Apply f a = MkApply (f a) deriving (Eq, Show, Functor, Foldable)
data Fix f a = MkFix (f (Fix f a)) deriving (Functor, Foldable)
data Compose f g a = MkCompose (f (g a)) deriving (Eq, Show, Functor, Foldable)
data Sum f a b = FLeft (f a) | FRight (f b) deriving (Eq, Show, Functor, Foldable)
data Prod f a b = FProd (f a) (f b) deriving (Eq, Show, Functor, Foldable)
data FList f a = FNil | FCons (f a) (f (FList f a)) deriving (Functor, Foldable)

-- Írjuk meg ezt a műveletet pár fenti típusra!
mapMSingle :: Monad m => (a -> m b) -> Single a -> m (Single b)
mapMSingle f (Single a) = f a >>= \b -> return $ Single b

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f (Tuple a b) = f a >>= \ma -> f b >>= \mb -> return $ Tuple ma mb

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple f (Quintuple a b c d e) = do
  ma <- f a
  mb <- f b
  mc <- f c
  md <- f d
  me <- f e
  return $ Quintuple ma mb mc md me

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe _ Nothing = return Nothing
mapMMaybe f (Just a) = Just <$> f a

-- Ehhez a mellékhatásos mappoláshoz viszont a Monád megkötés sokat enged meg
-- A monád fő művelete a (>>=) :: m a -> (a -> m b) -> m b
-- Ami mellékhatásos műveletek közti függőséged modellez.
-- Viszont mappolásnál az egyes elemek között eredmény alapú függőség nincs
-- Applicative típusosztály: A Functor és a Monád között van
{-
:i Applicative
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in ‘Control.Applicative’
-}
-- A típusosztály koncepciója az fmap művelet általánosítása tetszőleges paraméterű függvényre, pl.:
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- Ehhez viszont szükség van (egymástól független) mellékhatások kombinációjára
-- liftA műveletek csak liftA3-ig vannak standard libraryben, viszont arbitrary liftA írtható a <*> segítségével
-- pl.:
liftA4 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA4 func fa fb fc = func <$> fa <*> fb <*> fc
-- func :: a -> b -> c -> d
-- func <$> fa :: f (b -> c -> d)
-- func <$> fa <*> fb :: f (c -> d)
-- func <$> fa <*> fb <*> fc :: f d
-- Ezeket az ún app láncot fogjuk használni mapA írásnál is!
-- Írjuk meg a mapM műveletet Applicative segítségével
-- Az algoritmus ugyanaz mint a funktornál csak függvényalkalmazás helyett <*> és független értékek esetén pure
mapA :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA f Nil = pure Nil
mapA f (Cons a as) = Cons <$> f a <*> mapA f as

-- Ez a mappolhatósági tulajdonság lesz az úgynevezett Traversable típusosztály
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
        -- Defined in ‘Data.Traversable’
-}

instance Traversable Single where
  sequenceA :: Applicative f => Single (f a) -> f (Single a)
  sequenceA (Single fa) = Single <$> fa
  traverse :: Applicative f => (a -> f b) -> Single a -> f (Single b)
  traverse f (Single a) = Single <$> f a

instance Traversable Tuple where
  sequenceA :: Applicative f => Tuple (f a) -> f (Tuple a)
  sequenceA (Tuple fa fb) = Tuple <$> fa <*> fb
  traverse :: Applicative f => (a -> f b) -> Tuple a -> f (Tuple b)
  traverse f (Tuple a b) = Tuple <$> f a <*> f b

instance Traversable Quintuple where
  sequenceA :: Applicative f => Quintuple (f a) -> f (Quintuple a)
  sequenceA (Quintuple fa fb fc fd fe) = Quintuple <$> fa <*> fb <*> fc <*> fd <*> fe
  traverse :: Applicative f => (a -> f b) -> Quintuple a -> f (Quintuple b)
  traverse f (Quintuple a b c d e) = Quintuple <$> f a <*> f b <*> f c <*> f d <*> f e

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where
  sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
  sequenceA Nothing = pure Nothing
  sequenceA (Just fa) = Just <$> fa
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse f Nothing = pure Nothing
  traverse f (Just a) = Just <$> f a

instance Traversable NonEmpty where
  sequenceA :: Applicative f => NonEmpty (f a) -> f (NonEmpty a)
  sequenceA (NECons fa fas) = NECons <$> fa <*> sequenceA fas
  sequenceA (Last fa ) = Last <$> fa

instance Traversable NonEmpty2 where
  sequenceA :: Applicative f => NonEmpty2 (f a) -> f (NonEmpty2 a)
  sequenceA (NECons2 a xs) = NECons2 <$> a <*> sequenceA xs
  traverse :: Applicative f => (a -> f b) -> NonEmpty2 a -> f (NonEmpty2 b)
  traverse f (NECons2 a xs) = NECons2 <$> f a <*> traverse f xs

instance Traversable Tree where
  sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node a b c) = Node <$> sequenceA a <*> b <*> sequenceA c
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node a b c) = Node <$> traverse f a <*> f b <*> traverse f c

instance Traversable (Either fixed) where
  sequenceA :: Applicative f => Either fixed (f a) -> f (Either fixed a)
  sequenceA (Left a) = pure $ Left a
  sequenceA (Right fa) = Right <$> fa
  traverse :: Applicative f => (a -> f b) -> Either fixed a -> f (Either fixed b)
  traverse f (Left a) = pure $ Left a
  traverse f (Right a) = Right <$> f a


instance Traversable (BiTuple fixed) where
  sequenceA :: Applicative f => BiTuple fixed (f a) -> f (BiTuple fixed a)
  sequenceA (BiTuple a fa) = BiTuple a <$> fa

instance Traversable (TriEither fixed1 fixed2) where
  sequenceA :: Applicative f => TriEither fixed1 fixed2 (f a) -> f (TriEither fixed1 fixed2 a)
  sequenceA (LeftT fixed1) = pure $ LeftT fixed1
  sequenceA (MiddleT fixed2) = pure $ MiddleT fixed2
  sequenceA (RightT fa) = RightT <$> fa

instance Traversable (BiList fixed) where
  sequenceA :: Applicative f => BiList fixed (f a) -> f (BiList fixed a)
  sequenceA ABNill = pure ABNill
  sequenceA (ACons a as) = ACons a <$> sequenceA as
  sequenceA (BCons fb as) = BCons <$> fb <*> sequenceA as

-- Magasabbrendű megkötések
instance Traversable f => Traversable (Apply f) where
  sequenceA :: (Traversable f, Applicative f1) => Apply f (f1 a) -> f1 (Apply f a)
  sequenceA (MkApply fa) = MkApply <$> sequenceA fa

instance Traversable f => Traversable (Fix f) where
  sequenceA :: (Traversable f, Applicative f1) => Fix f (f1 a) -> f1 (Fix f a)
  sequenceA (MkFix f) = undefined -- TODO

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  sequenceA :: (Traversable f, Traversable g, Applicative f1) => Compose f g (f1 a) -> f1 (Compose f g a)
  sequenceA (MkCompose fga) = MkCompose <$> traverse sequenceA fga

instance Traversable f => Traversable (Sum f fixed) where
  sequenceA :: (Traversable f, Applicative f1) => Sum f fixed (f1 a) -> f1 (Sum f fixed a)
  sequenceA (FLeft a) = pure $ FLeft a
  sequenceA (FRight b) = FRight <$> sequenceA b

instance Traversable f => Traversable (Prod f fixed) where
  sequenceA :: (Traversable f, Applicative f1) => Prod f fixed (f1 a) -> f1 (Prod f fixed a)
  sequenceA (FProd fa fb) = FProd fa <$> sequenceA fb

instance Traversable f => Traversable (FList f) where
  sequenceA :: (Traversable f, Applicative f1) => FList f (f1 a) -> f1 (FList f a)
  sequenceA FNil = pure FNil
  sequenceA (FCons fa fas) = FCons <$> sequenceA fa <*> traverse sequenceA fas

-- Kiegészítő tananyag: Applicative Do

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
