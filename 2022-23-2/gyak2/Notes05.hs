
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Következő feladat:
--   Egyszerűbb függvény: Maybe vagy IO monád használata

--------------------------------------------------------------------------------

data List a = Empty | Cons1 a (List a) | Cons2 a a (List a) deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f Empty            = Empty
  fmap f (Cons1 a as)     = Cons1 (f a) (fmap f as)
  fmap f (Cons2 a1 a2 as) = Cons2 (f a1) (f a2) (fmap f as)

--------------------------------------------------------------------------------

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

-- filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- filterMaybe = undefined

{-
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = case f a of    -- fmap f (Leaf a) = Leaf (f a)
  Nothing -> Nothing
  Just b  -> Just (Leaf b)
mapMaybeTree f (Node t1 t2 t3) =
  case (mapMaybeTree f t1, mapMaybeTree f t2, mapMaybeTree f t3) of
    (Just t1, Just t2, Just t3) -> Just (Node t1 t2 t3)
    _                           -> Nothing
-}

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) =
  f a >>= \b ->
  return (Leaf b)

  -- ma >>= f = case ma of
  --   Nothing -> Nothing
  --   Just a  -> f a

  -- return = Just

  -- behelyettesítés:
  -- case (f a) of
  --    Nothing -> Nothing
  --    Just b  -> (\b -> return (Leaf b)) b

  -- case (f a) of
  --    Nothing -> Nothing
  --    Just b  -> return (Leaf b)

  -- case f a of
  --    Nothing -> Nothing
  --    Just b  -> Just (Leaf b)

mapMaybeTree f (Node t1 t2 t3) =
  mapMaybeTree f t1 >>= \t1 ->
  mapMaybeTree f t2 >>= \t2 ->
  mapMaybeTree f t3 >>= \t3 ->
  return (Node t1 t2 t3)

-- Szorgalmi: behelyettesíteni a >>= definíciókat
-- Zárójelezés:

{-
  mapMaybeTree f t1 >>= (\t1 ->
  mapMaybeTree f t2 >>= (\t2 ->
  mapMaybeTree f t3 >>= (\t3 ->
  return (Node t1 t2 t3))))
-}


zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) =
  f a b >>= \c ->
  zipWithMaybe f as bs >>= \cs ->
  return (c:cs)
zipWithMaybe _ _ _ = return []

-- -- -- Definiáld újra a függvényeket do notáció használatával!
-- filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- filterMaybe2 = undefined

mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 f (Leaf a) = do
  b <- f a
  return (Leaf b)
mapMaybeTree2 f (Node t1 t2 t3) = do
  t1 <- mapMaybeTree2 f t1
  t2 <- mapMaybeTree2 f t2
  t3 <- mapMaybeTree2 f t3
  return (Node t1 t2 t3)

zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithMaybe2 f as bs
  return (c:cs)
zipWithMaybe2 _ _ _ = return []

-- Definiáld újra a függvényeket tetszőleges monádra!
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = return []
filterM f (a:as) = do
  b <- f a
  if b then do
    as <- filterM f as
    return (a:as)
  else do
    filterM f as

-- filterM tetszőleges monádra működik

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f (Leaf a) = do
  b <- f a
  return (Leaf b)
mapTreeM f (Node t1 t2 t3) = do
  t1 <- mapTreeM f t1
  t2 <- mapTreeM f t2
  t3 <- mapTreeM f t3
  return (Node t1 t2 t3)

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithM f as bs
  return (c:cs)
zipWithM _ _ _ = return []


-- IO monád
--------------------------------------------------------------------------------

-- primitív (beépített)
-- IO-ban elérhetők input-output műveletek
-- program main függvénye  IO ()

-- main :: IO ()

-- "unit típus": üres tuple típus
--   egy értéke van: ()
-- 2-tuple: (a, b) típus
--          (x, y)
--           () típus
--           () érték

-- f :: IO ()        "f egy olyan művelet, aminek IO mellékhatása lehet és () értékkel
--                    tér vissza"

-- f :: IO a         "mellékhatása IO, visszatérési értéke "a" típusú"

-- x :: Maybe a      "mellékhatás Maybe, visszatérési érték típusa "a"

-- x :: m a    ahol Monad m


-- Használjuk a következő standard függvényeket, illetve a do notációt.

-- getLine  :: IO String             -- beolvas egy sort

-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- print    :: Show a => a -> IO ()  -- kinyomtat egy értéket amire van Show instance

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- (>>)   :: IO a -> IO b -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a' és 'z'
-- közötti karakterek számát.
io1 :: IO Int
io1 = do
  l <- getLine
  return $ length $ filter (\c -> 'a' <= c && c <= 'z') l

io1' :: IO Int
io1' = do
  l <- getLine

  -- let használható do blokkban, nem kell "in"
  let letterNum :: Int
      letterNum = length $ filter (\c -> 'a' <= c && c <= 'z') l

  let foo :: Int -> Int
      foo x = x * x

  return letterNum

  -- let: tiszta (mellékhatás-mentes) értékadás
  -- bind: művelet végrehajtás, végeredményre hivatkozás


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
