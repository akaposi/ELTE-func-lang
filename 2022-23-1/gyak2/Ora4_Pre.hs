{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- Ha ez valakinek nem fordul szedje ki
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Ora4 where

import Control.Monad
import Prelude hiding (Maybe(..), Either(..))

-- Mellékhatás modellezése
-- pl Maybe
-- Nothing a hiba eset
data Maybe a = Just a | Nothing deriving Functor
-- pl Either
-- Left a hibaüzenetek esete
data Either a b = Left a | Right b deriving Functor


-- Ha egy hiba is van, akkor az egész mappolás elhasal
-- case-of is nagyon hasznos ennél
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe = undefined

-- e a hibaüzenet komponens
mapEither :: (a -> Either b e) -> [a] -> Either [b] e
mapEither = undefined

-- Ilyesfajta művelektre van a "bind" függvény
-- A maybe belső eleme kihat a hülső struktúrára (mellékhatás) -> Justból is lehet Nothing
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = undefined

bindEither :: (a -> Either e b) -> Either e a -> Either e b
bindEither = undefined

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
    return = undefined
    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
    m >>= f = undefined

instance Applicative (Either e) where
    (<*>) = ap
    pure = return

instance Monad (Either e) where
    return :: a -> Either e a
    return = undefined
    (>>=)  :: Either e a -> (a -> Either e b) -> Either e b
    eth >>= f = undefined

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

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' = undefined

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' = undefined

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

-- Írjuk meg a fenti függvényeket do-notációval

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = undefined

filterM'' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM'' = undefined

replicateM'' :: Monad m => Int -> [a] -> m [a]
replicateM'' = undefined

