{-# language DeriveFunctor #-}


-- Írd át az összes következő függvényt úgy, hogy
-- a (Monad m) helyett (Applicative m) legyen használva.
-- A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

f1 :: Monad m => b -> m a -> m b
f1 b ma = do
  a <- ma
  return b

f2 :: Monad m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = do
  b <- mb
  if b then do {a <- ma; ma'; return a}
       else do {ma; ma'}

f3 :: Monad m => (a -> m b) -> [a] -> m [b]
f3 f []     = return []
f3 f (a:as) = do
  b  <- f a
  bs <- f3 f as
  return (b : bs)

f4 :: Monad m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = return []
f4 f (a:as) = do
  b <- f a
  if b then do {as' <- f4 f as; return (a:as')}
       else f4 f as

f5 :: Monad m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md = do
  md
  c <- mc
  mb
  a <- ma
  return (a, c)

f6 :: Monad m => (a -> b) -> m a -> m b
f6 f ma = do
  a <- ma
  return (f a)

f7 :: Monad m => m (a -> b) -> m a -> m b
f7 mf ma = do
  f <- mf
  a <- ma
  return (f a)

f8 :: Monad m => m a -> m b -> m b
f8 ma mb = ma >> mb


-- Implementáld a következő instance-okat!
data Const a b        = Const a b             deriving Functor
newtype Id a          = Id a                  deriving Functor
data Product f g a    = Product (f a) (g a)   deriving Functor
newtype Compose f g a = Compose (f (g a))     deriving Functor

instance Monoid a => Applicative (Const a) where
  pure = undefined
  (<*>) = undefined

instance Applicative Id where
  pure = undefined
  (<*>) = undefined

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure = undefined
  (<*>) = undefined

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = undefined
  (<*>) = undefined


-- Implementáld a következő függvényeket
first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

left :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
left = undefined

right :: Applicative f => (a -> f b) -> Either c a -> f (Either c b)
right = undefined

g1 :: Applicative f => (a -> f b) -> Either (a, c) d -> f (Either (b, c) d)
g1 = undefined

g2 :: Applicative f => (a -> f b) -> (Either a c, d) -> f (Either b c, d)
g2 = undefined
