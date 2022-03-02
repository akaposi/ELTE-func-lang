{-# language InstanceSigs #-}

-- következő canvas: egyszerű (nem rekurzív)
--   Maybe függvény (magasabbrendű függvény + tuple)
--   bind-al megoldható, de nem muszáj azzal oldani


-- canvas
------------------------------------------------------------

data D a = D1 Bool a | D2 Int a Int | D3 Bool
  deriving (Show, Eq)

instance Functor D where
  fmap :: (a -> b) -> D a -> D b
  fmap f (D1 b a)     = D1 b (f a)
  fmap f (D2 n1 a n2) = D2 n1 (f a) n2
  fmap f (D3 b)       = D3 b
  -- fmap f (D3 b)    = D3 b

data Const a b = Const a  -- (konstans funktor)
                          -- const x y = y

instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c  -- típust változtató fmap


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
    Nothing -> Nothing
    Just as -> if b then Just (a:as)
                    else Just as

-- filter f []     = []
-- filter f (a:as) =
--    if f a then a : filter f as
--           else filter f as


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving Show

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = fmap Leaf (f a)    -- _ :: b -> Tree b
  -- case f a of
  --   Nothing -> Nothing
  --   Just b  -> Just (Leaf b)
mapMaybeTree f (Node t1 t2 t3) =

  case mapMaybeTree f t1 of
    Nothing  -> Nothing
    Just t1' -> case mapMaybeTree f t2 of
      Nothing -> Nothing
      Just t2' -> case mapMaybeTree f t3 of
        Nothing -> Nothing
        Just t3' -> Just (Node t1' t2' t3')

  -- case (mapMaybeTree f t1, mapMaybeTree f t2, mapMaybeTree f t3) of
  --   (Just t1', Just t2', Just t3') -> Just (Node t1' t2' t3')
  --   _ -> Nothing

  -- isNothing :: Maybe a -> Bool
  -- fromJust  :: Maybe a -> a        -- import Data.Maybe

  -- null       :: [a] -> Bool
  -- head, tail                  -- nem teljes függvények!

  -- case xs of [] -> _; x:xs -> _    (defenzív programozás)


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) = case f a b of
  Nothing -> Nothing
  Just c  -> case zipWithMaybe f as bs of
    Nothing -> Nothing
    Just cs -> Just (c:cs)

zipWithMaybe f _ _ = Just []



-- fmap   :: (a -> b) -> Maybe a -> Maybe b
-- (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b    -- (bind)
-- return :: a -> Maybe a                            -- (Just)


-- Definiáld újra az előbbi három
-- feladatot a Maybe monád instance használatával.

-- ("monádikus": Monad instance-t használ)

-- Nothing >>= f = Nothing
-- Just a  >>= f = f a

--

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f []     = return []
filterMaybe' f (a:as) =
  f a >>= \b ->                        -- b :: Bool
  filterMaybe' f as >>= \as ->         -- as :: [a]
  if b then return (a:as)
       else return as

-- >>= bal oldalán (Maybe a)
--     jobb oldalán  \x -> ...     (x hivatkozik a Just-ban lévő mezőre)

-- cukorka: "do notáció"

filterMaybe'' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe'' f []     = return []
filterMaybe'' f (a:as) = do
  b <- f a
  as <- filterMaybe'' f as
  if b then return (a:as)
       else return as

-- do szintaxis fordítása

-- do x <- ma              ma >>= \x ->
--    mb                   mb

-- do ma                   ma >>= \_ ->
--    mb                   mb

  -- f a >>= \b ->                         -- b :: Bool
  -- filterMaybe'' f as >>= \as ->         -- as :: [a]
  -- if b then return (a:as)
  --      else return as

    -- case f a of
  --   Nothing -> Nothing
  --   Just b  -> Just (Leaf b)
-- mapMaybeTree f (Node t1 t2 t3) =

--   case mapMaybeTree f t1 of
--     Nothing  -> Nothing
--     Just t1' -> case mapMaybeTree f t2 of
--       Nothing -> Nothing
--       Just t2' -> case mapMaybeTree f t3 of
--         Nothing -> Nothing
--         Just t3' -> Just (Node t1' t2' t3')

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a) = do
  b <- f a
  return (Leaf b)
  -- case f a of
  --   Nothing -> Nothing
  --   Just b  -> Just (Leaf b)
mapMaybeTree' f (Node t1 t2 t3) = do
  t1 <- mapMaybeTree f t1    -- változó árnyékolás
  t2 <- mapMaybeTree f t2
  t3 <- mapMaybeTree f t3
  return (Node t1 t2 t3)

--   case mapMaybeTree f t1 of
--     Nothing  -> Nothing
--     Just t1' -> case mapMaybeTree f t2 of
--       Nothing -> Nothing
--       Just t2' -> case mapMaybeTree f t3 of
--         Nothing -> Nothing
--         Just t3' -> Just (Node t1' t2' t3')

zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' = undefined




-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String             -- beolvas
-- print    :: Show a => a -> IO ()  -- kinyomtat értéket
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
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


-- bónusz funktor feladatok múlt hétről
--------------------------------------------------------------------------------


-- Bónusz feladatok
--------------------------------------------------------------------------------

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = undefined

apply :: Functor f => f (a -> b) -> a -> f b
apply = undefined

first :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
first = undefined

second :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
second = undefined

data Sum f g a = Inl (f a) | Inr (g a) deriving Show
data Product f g a = Product (f a) (g a) deriving Show
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap = undefined

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap = undefined


-- bónusz bónusz: mire használható ez a függvény? Tipp: a megoldáshoz
-- rekurzió szükséges.
löb :: Functor f => f (f a -> a) -> f a
löb = undefined

-- bónusz bónusz 2:
newtype Fix f = Fix (f (Fix f))

fold :: Functor f => (f a -> a) -> Fix f -> a
fold = undefined
