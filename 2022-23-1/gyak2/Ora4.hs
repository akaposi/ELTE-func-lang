{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- Ha ez valakinek nem fordul szedje ki
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Ora4 where

import Control.Monad
import Prelude hiding (Maybe(..), Either(..))

-- Vegyük az alábbi típust
data MyFavouriteType a = Three a a a | Recurse (MyFavouriteType a) | RecurseTwice (MyFavouriteType a) (MyFavouriteType a)
 
-- Definiáljunk rá Functor instanceot!
-- Deriving semmilyen formában nem használható!
-- 2 pont
instance Functor MyFavouriteType where
    fmap f (Three a b c) = Three (f a) (f b) (f c)
    fmap f (Recurse a) = Recurse $ fmap f a
    fmap f (RecurseTwice a b) = RecurseTwice (fmap f a) (fmap f b)

-- Mellékhatás modellezése
-- pl Maybe
-- Nothing a hiba eset
data Maybe a = Just a | Nothing deriving (Functor, Show)
-- pl Either
-- Left a hibaüzenetek esete
data Either a b = Left a | Right b deriving (Functor, Show)


-- Ha egy hiba is van, akkor az egész mappolás elhasal
-- case-of is nagyon hasznos ennél
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f [] = Just []
mapMaybe f (x:xs) = case mapMaybe f xs of
    Nothing -> Nothing
    Just ys -> case f x of
        Nothing -> Nothing
        Just y  -> Just (y : ys)

-- e a hibaüzenet komponens
mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f [] = Right []
mapEither f (x:xs) = case mapEither f xs of
    Left e   -> Left e
    Right ys -> case f x of
        Left e' -> Left e'
        Right y -> Right (y : ys) 

filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe p [] = Just []
filterMaybe p (x:xs) = case filterMaybe p xs of
    Nothing -> Nothing
    Just ys -> case p x of
        Nothing -> Nothing
        Just y -> if y then Just (x:ys) else Just ys

filterEither :: (a -> Either e Bool) -> [a] -> Either e [a]
filterEither p [] = Right []
filterEither p (x:xs) = case filterEither p xs of
    Left e -> Left e
    Right ys -> case p x of
        Left e' -> Left e'
        Right b -> if b then Right (x : ys) else Right ys

-- Ilyesfajta művelektre van a "bind" függvény
-- A maybe belső eleme kihat a hülső struktúrára (mellékhatás) -> Justból is lehet Nothing
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f Nothing = Nothing
bindMaybe f (Just a) = f a

bindEither :: (a -> Either e b) -> Either e a -> Either e b
bindEither f (Left e) = Left e
bindEither f (Right a) = f a

returnMaybe :: a -> Maybe a
returnMaybe a = Just a

returnEither :: a -> Either e a
returnEither a = Right a

{-
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
-}

-- Az applikatív kicsitt absztraktabb dolog, kövi órán vesszük
-- Annyi releváns, hogy minden applicative egy functor is tehát
-- class Functor f => Applicative f where

instance Applicative Maybe where
    (<*>) = ap
    pure = return

instance Monad Maybe where
    return :: a -> Maybe a
    return = Just
    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
    m >>= f = bindMaybe f m

instance Applicative (Either e) where
    (<*>) = ap
    pure = return

instance Monad (Either e) where
    return :: a -> Either e a
    return = returnEither
    (>>=)  :: Either e a -> (a -> Either e b) -> Either e b
    eth >>= f = bindEither f eth

-- Milyen műveletek járnak a Monad-al?
-- Pl konstans bind 
-- (>>) :: Monad m => m a -> m b -> m b
-- a >> b = a >>= \_ -> b
-- Szekvenciálásra jó, amikor nem fontos az előző monád

-- Nagyon sok (see Control.Monad)
-- Általános függvények monadikus variánsa
{-
    map   ::            (a ->   b) -> [a] ->   [b]
    mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
    mapM_ :: Monad m => (a -> m b) -> [a] -> m ()  <- eldobjuk a listát, csak a mellékhatás érdekel

    filter  ::            (a ->   Bool) -> [a] ->   [a]
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

    Ebből filterM_ nincs de lehetne simán írni

-}

-- a mapMaybe és mapEither feljebb lényegében mapM-ek voltak

-- írjunk még monád fv-eket

-- replicate   ::            Int ->   a ->   [a]
-- replicateM  :: Monad m => Int -> m a -> m [a]
-- replicateM_ :: Monad m => Int -> m a -> m ()

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- return :: Monad m => a -> m a
-- (>>) :: Monad m => m a -> m b -> m b
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' i m | i <= 0 = return []
replicateM' i m = m >>= \a -> replicateM' (i - 1) m >>= \as -> return (a:as)


filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = p x >>= \b -> filterM' p xs >>= \ys -> if b then return (x:ys) else return ys

{-
filterM' (\a -> if a > 5 then Just True else (if a < 0 then Just False else Nothing)) [-1, -2, 6, 7]
x = -1
xs = [-2, 6, 7]
p x = Just False
Just False >>= \b -> ... 
b = False
filterM' p xs = Just [6,7]
x = -2
xs = [6,7]
p x = Just False
b = False
filterM' p xs = Just [6,7]
x = 6
xs = [7]
p x = Just True
b = True
ys = [7]
filterm' p xs = Just [7]
x = 7
xs = []
p x = Just True
b = True
ys = []
filterM' p xs = Just []

-}

-- Most még csak tesztelésre:
-- I/O haskellben: IO monád
-- standard konzol műveletek
-- getLine  :: IO String
-- print    :: Show a => a -> IO ()
-- putStr   :: String -> IO ()
-- putStrLn :: String -> IO ()
-- readLn   :: Read a => IO a

-- Ezeket lehet komibálni >>=-al
-- (readLn :: IO Int) >>= print
-- beolvas egy számot majd kiírja terminálra - 1x próbálkozik majd elhasal

-- do-notáció: Imperatív monád írási stílus
{-
do
    b <- a
≡
a >>= \b -> ...

do
    a
    b
≡
a >> b

do
    b <- a
    c <- a
    return $ b + c
≡
a >>= \b -> a >>= \c -> return $ b + c
-}

readAndPrint :: IO ()
readAndPrint = (readLn :: IO Int) >>= print

readAndPrint' :: IO ()
readAndPrint' = do
    i <- readLn :: IO Int
    print i

-- Írjuk meg a fenti függvényeket do-notációval

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do
    y <- f x -- f = getLine
    ys <- mapM' f xs
    return (y:ys)



filterM'' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM'' = undefined

replicateM'' :: Monad m => Int -> m a -> m [a]
replicateM'' i a | i <= 0 = return []
replicateM'' i a = do
    b <- a
    bs <- replicateM'' (i - 1) a
    return (b:bs)

