{-# LANGUAGE DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving, ApplicativeDo #-}
module Gy08 where

import Prelude hiding (Maybe(..), Either(..))

-- Definiáljuk egy függvényt, amely egy "mellékhatásos" függvényt végig mappol egy listán
--                                          v az m mellékhatást összegyűjtjük
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f [] = return []
mapMList f (x : xs) = do
  x' <- f x
  xs' <- mapMList f xs
  return (x' : xs')
  

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
mapMSingle f (Single a) = do
  a' <- f a
  return (Single a')

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f (Tuple a b) = do
  a' <- f a
  b' <- f b
  return (Tuple a' b')

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple f (Quintuple a b c d e) = do
  a' <- f a
  b' <- f b
  c' <- f c
  d' <- f d
  e' <- f e
  return (Quintuple a' b' c' d' e')

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe f Nothing = return Nothing
mapMMaybe f (Just a) = do
  a' <- f a
  return (Just a')

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
mapA f (Cons x xs) = Cons <$> f x <*> mapA f xs

mapA' :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA' f Nil = pure Nil
mapA' f (Cons x xs) = do
  x' <- f x
  xs' <- mapA' f xs
  pure (Cons x' xs')

{-

Ha nincs konstruktor paraméter: akkor pure <constructor>
Ha van: akkor <constructor> <$> ...
- Ha a paraméter típusa == a: f a
- Ha ... tartalmaz a-t: traverse f xs
- Ha független: ???

Ha ezután van még paraméter: <*> ...
-}


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

instance Traversable Tuple where

instance Traversable Quintuple where
  traverse f (Quintuple a b c d e) = Quintuple <$> f a <*> f b <*> f c <*> f d <*> f e

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where

instance Traversable NonEmpty where
  traverse f (Last a) = Last <$> f a
  traverse f (NECons x xs) = NECons <$> f x <*> traverse f xs

instance Traversable NonEmpty2 where

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

-- * -> *
instance Traversable (Either fixed) where
  traverse f (Left fixed) = Left <$> pure fixed -- == pure (Left fixed)
  traverse f (Right a) = Right <$> f a
  
instance Traversable (BiTuple fixed) where

instance Traversable (TriEither fixed1 fixed2) where

instance Traversable (BiList fixed) where
  traverse f ABNill = pure ABNill
  traverse f (ACons a as) = ACons <$> pure a <*> traverse f as -- ACons a <$> traverse f as
  traverse f (BCons b bs) = BCons <$> f b <*> traverse f bs

-- Magasabbrendű megkötések
instance Traversable f => Traversable (Apply f) where
  traverse f (MkApply fa) = MkApply <$> traverse f fa
  -- fa :: Traversable f => f a

instance Traversable f => Traversable (Fix f) where
  traverse f (MkFix ffixfa) = MkFix <$> traverse (traverse f) ffixfa
  -- f (Fix f a)
  -- a -> f b
  -- Fix f a -> f (Fix f b)
  

instance (Traversable f, Traversable g) => Traversable (Compose f g) where

instance Traversable f => Traversable (Sum f fixed) where

instance Traversable f => Traversable (Prod f fixed) where

instance Traversable f => Traversable (FList f) where
  traverse f FNil = pure FNil
  traverse f (FCons fa fas) = FCons <$> traverse f fa <*> traverse (traverse f) fas

-- Kiegészítő tananyag: Applicative Do

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
