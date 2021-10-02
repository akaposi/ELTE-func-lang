
import Control.Monad -- replicateM_

------------------------------------------------------------

data D a = D1 a Int a | D2 Bool Bool a
  deriving (Show, Eq)

instance Functor D where
  fmap f (D1 a1 n a2) = D1 (f a1) n (f a2)
  fmap f (D2 b1 b2 a) = D2 b1 b2 (f a)

-- következő feladat:
--    egyszerű Maybe monád
--      (monád metódus + do notáció használatával)
--      (egyszerűbb mint alábbi három függvény)

-- Első házi feladat: legkorábban 2 hét múlva az első kiírása


-- Maybe monád motiváló feladatok
--------------------------------------------------------------------------------

-- Írd meg a következő függvényt. A függvény úgy működik,
-- mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
-- függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
-- a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing  -> Nothing
    Just as' -> if b then Just (a:as') else Just as'

-- (defenzív prog: teljesen lefedett mintákat használjunk)

-- filterMaybe f list
--   | elem Nothing $ map f list = Nothing
--   | otherwise = Just list

-- filterMaybe _ [] = Just []
-- filterMaybe f (x:xs)
--     | not (any (\y -> (f y) == Nothing ) (x:xs)) = Just (x:xs)
--     | otherwise = Nothing


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a)   = case f a of
  Nothing -> Nothing
  Just b  -> Just (Leaf b)

-- alternatív:
-- mapMaybeTree f (Leaf a) = fmap Leaf (f a)

mapMaybeTree f (Node l r) = case mapMaybeTree f l of
  Nothing -> Nothing
  Just l' -> case mapMaybeTree f r of
    Nothing -> Nothing
    Just r' -> Just (Node l' r')

-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) = case f a b of
  Nothing -> Nothing
  Just c  -> case zipWithMaybe f as bs of
    Nothing -> Nothing
    Just cs -> Just (c:cs)
zipWithMaybe f as bs = Just []

-- Monad Maybe
--   return :: a -> Maybe a
--   (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
--   fmap   :: (a -> b) -> Maybe a -> Maybe b

-- + szintaktikus cukor: do notáció

-- Definiáld újra az előbbi három
-- feladatot a Maybe monád instance használatával.
filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f []     = return []
filterMaybe' f (a:as) = do
       -- f a :: Maybe Bool
       -- b :: Bool
  b <- f a
  as' <- filterMaybe' f as
  if b then return (a:as')
       else return as'

filterMaybe'' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe'' f []     = return []
filterMaybe'' f (a:as) =
  f a >>= (\b ->
  filterMaybe'' f as >>= (\as' ->
  if b then return (a:as')
       else return as'))

  -- f a >>= \b ->
  -- filterMaybe'' f as >>= \as' ->
  -- if b then return (a:as')
  --      else return as'

  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  -- (>>=) Nothing  f = Nothing
  -- (>>=) (Just a) f = f a

{-
  -- f a :: Maybe Bool
  f a >>= (\b ->                     -- Bool -> Maybe [a]
  filterMaybe'' f as >>= (\as' ->    -- filterMaybe'' f as :: Maybe [a]
  if b then return (a:as')           -- as' :: [a]
       else return as'))
-}

  -- if b then return (a:as')       :: Maybe [a]
  --      else return as'

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a)   = fmap Leaf (f a)
mapMaybeTree' f (Node l r) = do
  l' <- mapMaybeTree' f l
  r' <- mapMaybeTree' f r
  return (Node l' r')

zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithMaybe' f as bs
  return (c:cs)
zipWithMaybe' f as bs = return []


-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- return :: a -> IO a

-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a'    és 'z'
-- közötti karakterek számát.
io1 :: IO ()
io1 =
  getLine >>= \line ->       -- getLine :: IO String, line :: String
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') line
     --   print $ length [c | c <- line, 'a' <= c && c <= 'z']

-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined

-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.
-- return, (>>=), fmap
-- (alternatív: do-notáció)
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


-- Functor bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined
