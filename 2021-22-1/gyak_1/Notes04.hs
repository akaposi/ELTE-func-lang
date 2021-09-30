
import Control.Monad -- replicateM_

-- Maybe monád motiváló feladatok
--------------------------------------------------------------------------------

-- Írd meg a következő függvényt. A függvény úgy működik,
-- mint a lista "filter", viszont ha a kapott (a -> Maybe Bool)
-- függvény valamely elemre Nothing-ot ad, akkor Nothing legyen
-- a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe = undefined


-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden
-- levelére, ha bármelyik alkalmazás Nothing-ot ad,
-- legyen az eredmény Nothing!
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined


-- Definiáld újra az előbbi három feladatot a Maybe monád instance használatával.
filterMaybe' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe' = undefined

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' = undefined

zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' = undefined


-- IO monád
--------------------------------------------------------------------------------

-- getLine  :: IO String
-- print    :: Show a => a -> IO ()
-- putStrLn :: String -> IO ()

-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a' és 'z'
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


--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de típushelyesen.
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
