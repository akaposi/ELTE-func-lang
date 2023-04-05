
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Eq, Show)

{-
instance Monad Maybe where
  return :: a -> Maybe a
  return a = Just a

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) Nothing  f = Nothing
  (>>=) (Just a) f = f a


 bind szintaxis                  do szintaxis
-------------------------------------------------
   ma >>= \x ->                   do x <- ma
   mb                                mb

   ma >>= \_ ->                   do ma
   mb                                mb
-}


-- Definiáld a függvényeket >>= használatával!
-- Ha bármelyik Maybe-t adó függvény Nothing-ot ad,
-- akkor a végeredmény is legyen Nothing.

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined

zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined

-- -- Definiáld újra a függvényeket do notáció használatával!
filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe2 = undefined

mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 = undefined

zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 = undefined

-- Definiáld újra a függvényeket tetszőleges monádra!
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM = undefined

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM = undefined


-- IO monád
--------------------------------------------------------------------------------

-- Használjuk a következő standard függvényeket, illetve a do notációt.

-- getLine  :: IO String             -- beolvas egy sort
-- print    :: Show a => a -> IO ()  -- kinyomtat egy értéket amire van Show instance
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- (>>)   :: IO a -> IO b -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a'    és 'z'
-- közötti karakterek számát.
io1 :: IO ()
io1 = undefined


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = undefined


--------------------------------------------------------------------------------
-- Definiáld a következő függvényeket tetszőlegesen,
-- de típushelyesen.

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
