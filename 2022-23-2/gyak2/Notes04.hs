{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Eq, Show)


-- Írd meg a következő függvényt. A függvény úgy működik, mint a lista "filter",
-- viszont ha a kapott (a -> Maybe Bool) függvény valamely elemre Nothing-ot ad,
-- akkor Nothing legyen a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []   -- filter f [] = []
filterMaybe f (a:as) = case f a of
  Nothing    -> Nothing
  Just True  -> case filterMaybe f as of
                  Nothing -> Nothing
                  Just as -> Just (a:as)
  Just False -> filterMaybe f as

-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden levelére, ha bármelyik
-- alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = case f a of    -- fmap f (Leaf a) = Leaf (f a)
  Nothing -> Nothing
  Just b  -> Just (Leaf b)
mapMaybeTree f (Node t1 t2 t3) =
  case (mapMaybeTree f t1, mapMaybeTree f t2, mapMaybeTree f t3) of
    (Just t1, Just t2, Just t3) -> Just (Node t1 t2 t3)
    _                           -> Nothing

-- A fenti trükk Eq-nél is hasznos lehet (tuple-re mintaillesztés):

-- (==) b1 b2 = case (b1, b2) of
--   (True, True)   -> True
--   (False, False) -> True
--   _              -> False

-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) = case f a b of
  Nothing -> Nothing
  Just c  -> case zipWithMaybe f as bs of
    Nothing -> Nothing
    Just cs -> Just (c:cs)
zipWithMaybe _ _ _ = Just []


-- Maybe monad
--------------------------------------------------------------------------------

-- ötlet: a sok "case" helyett, használjuk egy függvényt,
-- ami egy osztály metódus

-- (megszorítás: csak akkor írhatunk "instance Monad m"-et, ha már
--  definiálva van "instance Functor m")
-- (pl:    class Eq a => Ord a where ...)
-- (előny: ha "Ord a" megszorítás van, akkor "Eq a" automatikusan szintén van)

foo :: Ord a => a -> a -> Bool
foo x y = x == y

{-
class Functor m => Monad m where
  return :: a -> m a                  -- *nem* a "return" statement!
  (>>=)  :: m a -> (a -> m b) -> m b  -- kiejtése: "bind"
-}

{-
instance Monad Maybe where
  return :: a -> Maybe a
  return a = Just a

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) Nothing f  = Nothing
  (>>=) (Just a) f = f a

  (>>=) ma f = case ma of
     Nothing -> Nothing
     Just a  -> f a
-}

-- definiáld újra a függvényeket (>>=) függvény felhasználásával! Ne használj
-- mintaillesztést Maybe típusú értékekre!
filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe2 f []     = return []    -- return = Just
filterMaybe2 f (a:as) =
  f a >>= (\b -> case b of
    True  -> filterMaybe2 f as >>= (\as -> return (a:as))
    False -> filterMaybe2 f as)
-- Mellékhatás: Nothing propagálása

-- szintaktikus cukor a (>>=)-ra

-- definiáld újra a függvényeket do notáció használatával!
filterMaybe3 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe3 f []     = return []
filterMaybe3 f (a:as) = do
  b <- f a
  case b of
    True -> do
      as <- filterMaybe3 f as
      return (a:as)
    False ->
      filterMaybe3 f as

-- "do" notáció: cukor (>>=) függvényre

--   bind szintaxis                  do szintaxis
-- ma >>= \x ->                   do x <- ma
-- mb                                mb

-- ma >>= \_ ->                   do ma
-- mb                                mb

-- példa:

foo' :: Maybe Int
foo' = do
  Nothing
  Nothing

-- foo' = Nothing >>= \_ -> Nothing

{-
foo() {
  throw();
  throw();
}

-}

foo2 :: Maybe Int
foo2 = do
  Nothing
  return 100

-- foo2 = Nothing >>= \_ -> Just 100  == Nothing

{-
foo2(){
  throw();
  return 100;
}
-}

-- Haskell-ben:
--   mellékhatás: user által definiált, több féle is lehet
--   alapból nincs
--     (f :: Int -> Int)             *nincs* mellékhatás
--     (f :: Int -> Maybe Int)        Nothing-dobás mint mellékhatás
--     (f :: Int -> Expection e Int)  "e" típusú kivétel dobás
--     (f :: ....)
--         (nem-determinizmus)
--         ("goto" mint mellékhatás)
--         do l <- getLabel
--            if ... then goto l else return ...

-- adjunk össze három Maybe Int-et, propagáljuk a hibát
bar :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
bar mx my mz = do
  x <- mx
  y <- my
  z <- mz
  return (x + y + z)

{-
-- imperatív pszeudokód:
filterMaybe(f, as) = case as of
  []   -> return [];
  a:as ->
    var b = f(a);    -- "f" függvény dobhat kivételt
    case b of
      True ->
        var as = filterMaybe3(f,as);
        return (a:as);          ";" helyett (>>=) írjuk Haskell-ben
      False ->
        return filterMaybe3(f,as);
-}

-- Imperatív programozás Haskell-ben: Monad osztály + szintaktikus cukorka
--
-- var x = st1; st2          "st2"-ben hivatkozhatunk "x"-re
-- st1 >>= \x -> st2



mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 = undefined

zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 = undefined


-- -- definiáld újra a függvényeket do notáció használatával!
-- filterMaybe3 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
-- filterMaybe3 = undefined

mapMaybeTree3 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree3 = undefined

zipWithMaybe3 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe3 = undefined


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
