{-# LANGUAGE ApplicativeDo, DeriveFoldable, DeriveFunctor, QuantifiedConstraints, StandaloneDeriving #-}
module Gy08_pre where

import Prelude hiding (Maybe(..), Either(..))

import Control.Monad.Except

-- Definiáljuk egy függvényt, amely egy "mellékhatásos" függvényt végig mappol egy listán
--                                          v az m mellékhatást összegyűjtjük
mapMList :: Monad m => (a -> m b) -> [a] -> m [b]
mapMList f []     = return []
mapMList f (x:xs) = 
  (f x) >>= \x' ->
  (mapMList f xs) >>= \xs' ->
  return (x' : xs')

f :: Int -> Except () Int
f i = if i == 0 then throwError () else return i

-- mapMList (putStrLn . show) [1..10]

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
  a' <-f a
  return (Single a')

mapMTuple :: Monad m => (a -> m b) -> Tuple a -> m (Tuple b)
mapMTuple f (Tuple a a') = do
  af <- f a
  af' <- f a'
  return (Tuple af af')

mapMQuintuple :: Monad m => (a -> m b) -> Quintuple a -> m (Quintuple b)
mapMQuintuple f (Quintuple x y z w v) = do
  fx <- f x
  fy <- f y
  fz <- f z
  fw <- f w
  fv <- f v
  return (Quintuple fx fy fz fw fv)

mapMMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMMaybe f Nothing  = return Nothing 
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
-- pure func :: f (a -> (b -> c -> d))
-- (pure func) <*> fa :: f (b -> c -> d)
-- ((pure func) <*> fa) <*> fb :: f (c -> d)

-- <$> :: (a -> (b -> c -> d)) -> f a -> f (b -> c -> d)

-- func <$> fa :: f (b -> c -> d)
-- (func <$> fa) <*> fb :: f (c -> d)
-- (func <$> fa <*> fb) <*> fc :: f d
-- Ezeket az ún app láncot fogjuk használni mapA írásnál is!
-- Írjuk meg a mapM műveletet Applicative segítségével
-- Az algoritmus ugyanaz mint a funktornál csak függvényalkalmazás helyett <*> és független értékek esetén pure
mapA :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA f Nil = pure Nil
mapA f (Cons x xs) = 
  let fb = f x in
  let re = mapA f xs in 
  Cons <$> fb <*> re
-- pure (Cons) :: f (b -> List b -> List b)

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
  traverse :: Applicative f => (a -> f b) -> Single a -> f (Single b)
  --fmap f (Single a) = Single (f a)
  -- fmap f ... = MkA  (f a) (f b) ....
  -- fmap f ... = (pure MkA) <*> (f a) <*> (f b) ....
  -- fmap f ... = MkA <$> (f a) <*> (f b) ....
  
  traverse f (Single a) = Single <$> (f a)
    -- Single :. b -> Single b 

instance Traversable Tuple where
  traverse f (Tuple x y) = Tuple <$> f x <*> f y

instance Traversable Quintuple where
  traverse f (Quintuple x y z w v) = Quintuple <$> f x <*> f y <*> f z <*> f w <*> f v

instance Traversable List where
  traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse = mapA

  sequenceA :: Applicative f => List (f a) -> f (List a)
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Traversable Maybe where
  traverse f Nothing  = pure Nothing 
  traverse f (Just a) = (pure Just) <*> (f a)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (Last a)      = pure Last <*> f a -- Last <$> f a
  traverse f (NECons a as) = pure NECons <*> f a <*> traverse f as 

instance Traversable NonEmpty2 where
  traverse f (NECons2 a as) = pure NECons2 <*> f a <*> traverse f as 

instance Traversable Tree where
  traverse f (Leaf a)   = Leaf  <$> f a
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r


instance Traversable (Either fixed) where
  traverse :: Applicative f => (a -> f b) -> Either fixed a -> f (Either fixed b)
  traverse f (Left fixed) = pure (Left fixed)
  traverse f (Right b)    = Right <$> f b

  sequenceA :: Applicative f => Either fixed (f a) -> f (Either fixed a)
  sequenceA (Left fixed) = pure (Left fixed)
  sequenceA (Right fa)   = pure Right <*> fa

instance Traversable (BiTuple fixed) where

instance Traversable (TriEither fixed1 fixed2) where

instance Traversable (BiList fixed) where

-- Magasabbrendű megkötések
instance Traversable f' => Traversable (Apply f') where

  traverse :: (Traversable t, Applicative f) => (a -> f b) -> Apply t a -> f (Apply t b)
  traverse g (MkApply fa) = MkApply <$> traverse g fa
  --traverse g (MkApply fa) = MkApply <$> (sequenceA $ (g <$>fa))

  sequenceA :: (Traversable t, Applicative f) => Apply t (f a) -> f (Apply t a)
  sequenceA (MkApply fa) = MkApply <$> sequenceA fa
  --traverse g (MkApply fa) = MkApply <$> (sequenceA $ (g <$>fa))

instance Traversable f' => Traversable (Fix f') where

  traverse :: (Traversable t, Applicative f) => (a -> f b) -> Fix t a -> f (Fix t b)
  traverse g (MkFix fa) = MkFix <$> (traverse (traverse g) fa)    

  -- traverse f a = sequenceA (f <$> a)
  -- traverse id (MkFix fa) = MkFix <$> (traverse (traverse id) fa)
  sequenceA (MkFix fa) = MkFix <$> (sequenceA (sequenceA <$> fa))


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse g (MkCompose fa) = MkCompose <$> (traverse (traverse g) fa)

instance Traversable f => Traversable (Sum f fixed) where
  traverse g (FLeft fb) = pure (FLeft fb)
  traverse g (FRight fa) = FRight <$> traverse g fa


instance Traversable f => Traversable (Prod f fixed) where
  traverse g (FProd fa fb) = FProd <$> (pure fa) <*> (traverse g fb)

-- data FList f a = FNil | FCons (f a) (f (FList f a)) deriving (Functor, Foldable)

instance Traversable f => Traversable (FList f) where
  traverse g FNil = pure FNil
  traverse g (FCons fa fListfa) = FCons <$> traverse g fa <*> traverse (traverse g) fListfa


-- Kiegészítő tananyag: Applicative Do

mapA' :: Applicative f => (a -> f b) -> List a -> f (List b)
mapA' f Nil = pure Nil
mapA' f (Cons a as) = do
  a' <- f a
  as' <- mapA' f as
  pure (Cons a' as')



-- :t \m -> do { x <- m 'a'; y <- m 'b'; return (x || y) }
testApp :: Applicative f => (Char -> f Bool) -> f Bool
testApp m = do
  x <- m 'a'
  y <- m 'b'
  return (x || y)
-- (\x y -> x || y) <$> m 'a' <*> m 'b'


-- Monad kell
-- :t \m -> do { x <- m True; y <- m x; return (x || y) }
testMonad :: Monad m => (Bool -> m Bool) -> m Bool
testMonad m = do 
  x <- m True
-- \-----\
--       V    
  y <- m x
  return (x || y)

-- ghc Gy08.hs -o 08.out -ddump-ds > dump.txt

id :: a -> a
id a = a

-- id @Int 10

main :: IO ()
main = return ()

-- Általánosan
{-
  ... = do
    p1 <- E1
    ...
    pn <- En
    return (f p1 p2 ...)
-}

-- Mágia, ignore me
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (Fix f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (Fix f a)
deriving instance (Eq a, forall a. Eq a => Eq (f a)) => Eq (FList f a)
deriving instance (Show a, forall a. Show a => Show (f a)) => Show (FList f a)
