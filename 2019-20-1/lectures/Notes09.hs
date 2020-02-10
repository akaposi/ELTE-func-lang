{-# language DeriveFunctor #-}


-- Írd át az összes következő függvényt úgy, hogy
-- a (Monad m) helyett (Applicative m) legyen használva.
-- A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

f1 :: Monad m => b -> m a -> m b
f1 b ma = do
  a <- ma
  return b

f1' :: Applicative m => b -> m a -> m b
f1' b ma = b <$ ma

f2 :: Monad m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = do
  b <- mb
  if b then do {a <- ma; ma'; return a}
       else do {ma; ma'}

f2' :: Applicative m => m Bool -> m a -> m a -> m a
f2' mb ma ma' =
  (\b a a' -> if b then a else a') <$> mb <*> ma <*> ma'

f3 :: Monad m => (a -> m b) -> [a] -> m [b]
f3 f []     = return []
f3 f (a:as) = do
  b  <- f a
  bs <- f3 f as
  return (b : bs)

f3' :: Applicative m => (a -> m b) -> [a] -> m [b]
f3' f []     = pure []
f3' f (a:as) = (:) <$> f a <*> f3' f as

f4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = return []
f4 f (a:as) = do
  b <- f a
  if b then do {as' <- f4 f as; return (a:as')}
       else f4 f as

f4' :: Applicative m => (a -> m Bool) -> [a] -> m [a]
f4' f [] = pure []
f4' f (a:as) =
  (\b as' -> if b then a:as' else as') <$> f a <*> f4' f as

f5 :: Monad m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md = do
  md
  c <- mc
  mb
  a <- ma
  return (a, c)

f5' :: Applicative m => m a -> m b -> m c -> m d -> m (a, c)
f5' ma mb mc md = (\d c b a -> (a, c)) <$> md <*> mc <*> mb <*> ma

f6 :: Monad m => (a -> b) -> m a -> m b
f6 f ma = do
  a <- ma
  return (f a)

f6' :: Applicative m => (a -> b) -> m a -> m b
f6' = fmap

f7 :: Monad m => m (a -> b) -> m a -> m b
f7 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f7' :: Applicative m => m (a -> b) -> m a -> m b
f7' = (<*>)

f8 :: Monad m => m a -> m b -> m b
f8 ma mb = ma >> mb

f8' :: Applicative m => m a -> m b -> m b
f8' = (*>)


-- Implementáld a következő instance-okat!
newtype Const a b     = Const a               deriving Functor
newtype Id a          = Id a                  deriving Functor
data Product f g a    = Product (f a) (g a)   deriving Functor
newtype Compose f g a = Compose (f (g a))     deriving Functor

instance Monoid a => Applicative (Const a) where
  pure a = Const mempty
  Const x <*> Const y = Const (x <> y)

instance Applicative Id where
  pure = Id
  Id f <*> Id a = Id (f a)

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure a = Product (pure a) (pure a)
  Product f g <*> Product x y = Product (f <*> x) (g <*> y)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose (pure (pure a))
  Compose fgf <*> Compose fga = Compose ((<*>) <$> fgf <*> fga)

-- Implementáld a következő függvényeket
first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first afb (a, c) = (\a -> (a, c)) <$> afb a

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second afb (c, a) = (\a -> (c, a)) <$> afb a

left :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
left afb (Left a)  = Left <$> afb a
left afb (Right b) = pure (Right b)

right :: Applicative f => (a -> f b) -> Either c a -> f (Either c b)
right afb (Left a)  = pure (Left a)
right afb (Right b) = Right <$> afb b

g1 :: Applicative f => (a -> f b) -> Either (a, c) d -> f (Either (b, c) d)
g1 = left . first

g2 :: Applicative f => (a -> f b) -> (Either a c, d) -> f (Either b c, d)
g2 = first . left
