
-- Köv BEAD:
--   Egyszerű IO program: lehet getLine, putStrLn, print
--   (Felteszek holnap github-ra IO feladatokat megoldással)


-- keresni: Monad tutorial  (50-60 Monad tutorial)   (Learn you a Haskell könyvben)

--   Maybe mellékhatás : Nothing propagál, mint egy kivétel
--      osztály : Monad
--      instance Monad Maybe
--         (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe  b      --
--         pure  :: a -> Maybe a                               -- pure = Just

-- (>>=) Nothing  _ = Nothing
-- (>>=) (Just a) f = f a

f2' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2' ma mb mc =
  ma >>= \a ->
  mb >>= \b ->
  mc >>= \c ->
  pure (a, b, c)

f2'' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2'' ma mb mc =
  ma >>= (\a ->
  mb >>= (\b ->
  mc >>= (\c ->
  pure (a, b, c))))

-- do notáció
f2''' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f2''' ma mb mc = do
  a <- ma
  b <- mb
  let x = 100
      y = 200
  c <- mc
  pure (a, b, c)

-- minden Monad instance: custom mellékhatást implementál
-- do notáció: imperatív notáció custom mellékhatásokra

-- be kell gyakorolni: mikor bind, mikor let, mi az ami "tiszta" mi az ami nem ("monádikus", "mellékhatásos")


-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing -> Nothing
    Just as -> if b then Just (a:as) else Just as

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f []     = pure []
filterMaybe' f (a:as) = do
  b   <- f a
  as' <- filterMaybe' f as
  if b then pure (a:as)
       else pure as

-- filterMaybe' f (a:as) =
--   f a               >>= \b ->
--   filterMaybe' f as >>= \as' ->
--   if b then pure (a:as)
--        else pure as

-- do notáció : (>>=) részt vesz + (>>) is

-- (>>) :: Monad m => m a -> m b -> m b          -- egymás után két műveletet végrehajtunk, második művelet értékét adjuk vissza
-- (>>) ma mb = ma >>= \_ -> mb


-- do notáció öszefoglalás:
--
--    x <- p1           p1 >>= \x ->
--    p2                p2

--    p1                p1 >>
--    p2                p2


-- 3. Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined

-- Írd meg az előbbi két függvényt Maybe Monad instance használatával
--  használj pure-t, (>>=)-ot vagy "do" notációt
--------------------------------------------------------------------------------

-- Írd meg a két fenti függvényt tetszőleges monádra!
-- metódusok:
--    pure  :: Monad m => a -> m a
--    (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- + fmap, mivel     class Functor m => Monad m     (Functor superclass-ja Monad-nak)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f []     = pure []
filterM f (a:as) = do
  b  <- f a
  as <- filterM f as
  if b then pure (a:as)
       else pure as


-- IO-val + Maybe-vel
--------------------------------------------------------------------------------

-- Maybe: kivétel
-- IO   : IO műveletek

-- p :: IO a            -- jelentés: p egy program, ami IO műveleteket tud elvégezni (printelni, beolvasni),
--                      --           visszatérési értékének típusa "a"

-- IO futtatni:
--   1. beütjük ghci-ben  (IO műveletet ghci futtat, és az eredményüket kinyomtatja)
--   2. definiáljuk "main :: IO ()" függvényt, és ghc-vel lefordítjuk a fájlt.

-- ghci-ben a parancssor az egy "do" blokk
--   >  line <- getLine

--  () : "unit" típus, értéke () :: ()
-- ha olyat látunk, hogy (p :: IO ()), ez olyan művelet, hogy hatása lehet, visszatérési értéke érdektelen ("void")

-- getLine  :: IO String                   -- beolvas egy sort stdin-ről, visszaadja
-- print    :: Show a => a -> IO ()        -- kinyomtat egy értéket az stdout-ra, amire van Show instance
-- putStrLn :: String -> IO ()             -- kinyomtat egy String-et, + newline

io1' :: IO ()
io1' = do
  line <- getLine
  putStrLn line
  putStrLn line

-- filterM példa Maybe-re és IO-ra is

-- filterM (\x -> getLine >>= \line -> pure (null line)) [0..10]         -- IO szűrés
-- filterM (\x -> if even x then Nothing else Just (x < 10)) [1, 3..20]  -- maybe szűrés


zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM = undefined


-- definiáld a következő függvényeket tetszőlegesen, de típushelyesen:

-- (feladat: függvényeket pure/bind/do notációval definiálni)
-- type hole-okat kéne nézni

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


-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String
-- print    :: Show a => a -> IO ()
-- putStrLn :: String -> IO ()

-- Írj egy függvényt, ami beolvas egy sort stdin-ről, majd a sort kinyomtatja annyiszor, ahány karakter van a sorban!
io1 :: IO ()
io1 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat stdin-ről, amíg a sor nem tartalmaz 'x' karaktert.
-- Ha a sorban 'x' van, akkor a program nyomtassa ki az összes eddig beolvasott sort és térjen vissza.
io2 :: IO ()
io2 = undefined
