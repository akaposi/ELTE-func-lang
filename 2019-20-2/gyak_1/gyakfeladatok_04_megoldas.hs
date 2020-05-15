
-- 1. Feladat: írd át az összes következő függvényt úgy, hogy
-- a (Monad m) helyett (Applicative m) legyen használva.
-- A függvények viselkedése ne változzon!
-- Tipp: Control.Applicative kombinátorait érdemes megnézni.

f1 :: Applicative m => b -> m a -> m b
f1 b ma = b <$ ma

f2 :: Applicative m => m Bool -> m a -> m a -> m a
f2 mb ma ma' = (\b a a' -> if b then a else a') <$> mb <*> ma <*> ma'

f3 :: Applicative m => (a -> m b) -> [a] -> m [b]
f3 f []     = pure []
f3 f (a:as) = (:) <$> f a <*> f3 f as

f4 :: Applicative m => (a -> m Bool) -> [a] -> m [a]
f4 f []     = pure []
f4 f (a:as) = (\b as -> if b then a:as else as) <$> f a <*> f4 f as

f5 :: Applicative m => m a -> m b -> m c -> m d -> m (a, c)
f5 ma mb mc md =
  (\d c b a -> (a, c)) <$> md <*> mc <*> mb <*> ma

f6 :: Applicative m => (a -> b) -> m a -> m b
f6 f ma = f <$> ma

f7 :: Applicative m => m (a -> b) -> m a -> m b
f7 mf ma = mf <*> ma

f8 :: Applicative m => m a -> m b -> m b
f8 ma mb = ma *> mb
