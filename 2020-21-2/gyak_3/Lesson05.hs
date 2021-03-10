{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}


{- composeMaybe variation -}

composeMaybe :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
composeMaybe = undefined


{- Monad instance variation -}

data Result a = Ok a | Err String

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap = undefined

instance Applicative Result where
  pure = undefined

  (<*>) :: Result (a -> b) -> Result a -> Result b
  (<*>) = undefined

instance Monad Result where
  return = undefined

  (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) = undefined

-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- (>=>) f g a = f a >>= g


{- IO -}

io :: IO ()
io = undefined

io' :: IO ()
io' = undefined


{- Standard library functions -}

sequence' :: Monad m => [m a] -> m [a]
sequence' = undefined

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' = undefined

forever' :: Monad m => m a -> m b
forever' = undefined

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined

mapM' :: Monad m => (a -> m b) -> [a] -> m [a]
mapM' = undefined

zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' = undefined

foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM' = undefined


{- Practice -}

f1 :: Monad m => (a -> b) -> m a -> m b
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

-- Írj egy függvényt, ami beolvas egy sort stdin-ről, majd a sort kinyomtatja
-- annyiszor, ahány karakter van a sorban!
io1 :: IO ()
io1 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat stdin-ről,
-- amíg a sor nem tartalmaz 'x' karaktert.
-- Ha a sorban 'x' van, akkor a program nyomtassa ki az összes
-- eddig beolvasott sort és térjen vissza.
io2 :: IO ()
io2 = undefined
