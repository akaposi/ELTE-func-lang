
-- 1. Írd meg a következő függvényt. A függvény úgy működik,
--    mint a "filter", viszont ha a kapott (a -> Maybe Bool)
--    függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
--    a végeredmény, egyébként Just <szűrt lista>

import Control.Monad

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f [] = Just []
filterMaybe f (x:xs) = case f x of
  Just b  -> case filterMaybe f xs of
    Nothing  -> Nothing
    Just xs' -> Just $ if b then x:xs' else xs'
  Nothing -> Nothing

-- 2. Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) =
  case f a of
    Nothing -> Nothing
    Just b  -> Just (Leaf b)
  -- fmap Leaf (f a)

mapMaybeTree f (Node l r) = case mapMaybeTree f l of
  Nothing -> Nothing
  Just l' -> case mapMaybeTree f r of
    Nothing -> Nothing
    Just r' -> Just (Node l' r')

-- szeretnénk nem kiírni
-- case x of Nothing -> ... Just _ ->     zajt

-- ha (Maybe a) Nothing, akkor eredmény Nothing
-- egyébként pedig meghívjuk a kapott függvényt
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  f = Nothing
bind (Just a) f = f a

-- bind :: m a -> (a -> m b) -> m b

filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' f []     = Just []
filterMaybe' f (a:as) =
  -- bind (f a) $ \b ->
  -- bind (filterMaybe' f as) $ \as' ->
  -- Just (if b then a:as' else as')
  bind (f a) (\b ->
  bind (filterMaybe' f as) (\as' ->
  Just (if b then a:as' else as')))

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a)   = Leaf <$> f a  -- (<$> = fmap)
mapMaybeTree' f (Node l r) =
  bind (mapMaybeTree' f l) $ \l' ->
  bind (mapMaybeTree' f r) $ \r' ->
  Just (Node l' r')


-- Monad
------------------------------------------------------------

-- szeretnénk olyan class-t használni, aminek a bind
-- az egyik metódusa
-- osztály: Monad

-- IO a
-- Functor IO
-- Monad IO

-- class Monad m where
--   (>>=)  :: m a -> (a -> m b) -> m b
--      -- mellékhatás propagálása
--   return :: a -> m a
--      -- érték visszaadása mellékhatás nélkül

-- instance Monad Maybe where
--   (>>=)  = bind
--   return = Just

helloWorld :: IO Bool
helloWorld =
  putStrLn "hello" >>= \_ ->
  putStrLn "worldp" >>= \_ ->
  return True

helloWorld2 :: IO ()
helloWorld2 =
  getLine >>= \l ->
  replicateM_ (read l) (putStrLn "Hello World")

-- maybeFoo :: Maybe Int

-- minden Monad instance valamilyen mellékhatást definiál
-- IO beépített dolog
-- minden más Monad instance user által lesz definiálva

-- Haskell-ben: alapból nincs mellékhatás
-- pl f :: Int -> Int
--    csak a típusra nézve tudod, hogy nincs mellékhatása
--    könnyű tesztelni, könnyű megérteni

-- mégis akarunk mellékhatást:
--   user deiniálja magának
