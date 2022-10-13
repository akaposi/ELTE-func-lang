{-# OPTIONS_GHC -Wincomplete-patterns -Wnoncanonical-monad-instances #-} -- kommenteld ki ha nem fordul
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Ora5Gyakorlas where

import Control.Applicative
import Control.Monad
import Control.Exception

-- Instance írás:

data AOrListA a    = A a    | List [a]                                 deriving (Functor, Show)
data AOrAOrListA a = A' a   | AOrListA' (AOrListA a)                   deriving (Functor, Show)
data CrazyType a   = C1     | C2 a | C3 Int Int Int | C4 (CrazyType a) deriving (Functor, Show)
data Tree a        = Leaf a | Branch (Tree a) (Tree a)                 deriving (Functor, Show)

instance Applicative AOrListA where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

instance Monad AOrListA where
    (>>=) = undefined

instance Applicative AOrAOrListA where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

instance Monad AOrAOrListA where
    (>>=) = undefined

instance Applicative CrazyType where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

instance Monad CrazyType where
    (>>=) = undefined

instance Applicative Tree where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

instance Monad Tree where
    (>>=) = undefined

-- Applikatív függvények:

-- TESZTELÉS:
-- Ha ezeket mind megírtad írd be a GHCi-be hogy:
-- runTests applicativeTests

productA :: (Num a, Applicative f) => [f a] -> f a
productA = undefined

fproductA :: (Num a, Applicative f, Foldable t) => t (f a) -> f a
fproductA = undefined

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA = undefined

apA2 :: Applicative f => f (a -> b -> c) -> f a -> f b -> f c
apA2 = undefined

-- Monád függvények:

-- TESZTELÉS:
-- Ha ezekt mind megírtad írd be a GHCi-be hogy:
-- runtests monadTests

triCompose :: Monad m => (a -> m b) -> (b -> m c) -> (c -> m d) -> (a -> m d)
triCompose = undefined

-- csak >>=-t és return-t használj
monadAp :: Monad m => m (a -> b) -> m a -> m b
monadAp = undefined

apBind :: Monad m => m (a -> m b) -> m a -> m b
apBind = undefined


{-





MANUÁLIS TESZTELŐ KÓD

INNENTÓL NEM KELL SEMMIT MEGOLDANI





-}

applicativeTests :: [(String, Bool)]
applicativeTests = [
    ("productA [Just 1, Just 2, Just 4] == Just 8", productA [Just 1, Just 2, Just 4] == Just 8),
    ("productA [Just 1, Just 2, Nothing] == Nothing", productA [Just 1, Just 2, Nothing] == Nothing),
    ("fproductA [Just 1, Just 2, Just 3] == Just 6", fproductA [Just 1, Just 2, Just 3] == Just 6),
    ("fproductA [Just 1, Just 2, Nothing] == Nothing", fproductA [Just 1, Just 2, Nothing] == Nothing),
    ("mapA (\\x -> case x of { 0 -> Nothing; _ -> Just (x + 1)}) [0,1,2,3] == Nothing ", mapA (\x -> case x of { 0 -> Nothing; _ -> Just (x + 1)}) [0,1,2,3] == Nothing),
    ("mapA (\\x -> case x of { 0 -> Nothing; _ -> Just (x + 1)}) [1,2,3] == Just [2,3,4] ", mapA (\x -> case x of { 0 -> Nothing; _ -> Just (x + 1)}) [1,2,3] == Just [2,3,4]),
    ("apA2 [(+),(*)] [1,2,3] [4,5,6] == [5,6,7,6,7,8,7,8,9,4,5,6,8,10,12,12,15,18]", apA2 [(+),(*)] [1,2,3] [4,5,6] == [5,6,7,6,7,8,7,8,9,4,5,6,8,10,12,12,15,18]) 
    ]

monadTests :: [(String, Bool)]
monadTests = [
    ("triCompose (\\a -> if a > 10 then Just a else Nothing) (\\b -> if b > 100 then Nothing else Just b) (\\c -> if c == 69 then Nothing else Just 72) 53 == Just 72", triCompose (\a -> if a > 10 then Just a else Nothing) (\b -> if b > 100 then Nothing else Just b) (\c -> if c == 69 then Nothing else Just 72) 53 == Just 72),
    ("triCompose (\\a -> if a > 10 then Just a else Nothing) (\\b -> if b > 100 then Nothing else Just b) (\\c -> if c == 69 then Nothing else Just 72) 69 == Nothing", triCompose (\a -> if a > 10 then Just a else Nothing) (\b -> if b > 100 then Nothing else Just b) (\c -> if c == 69 then Nothing else Just 72) 69 == Nothing),
    ("monadAp [(+1), (*2)] [] == []", monadAp [(+1), (*2)] [] == []),
    ("monadAp [(+1), (*2)] [1,2,3] == [2,3,4,2,4,6]", monadAp [(+1), (*2)] [1,2,3] == [2,3,4,2,4,6]),
    ("apBind [replicate 5, (:[])] [1,2,3] == [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,1,2,3]", apBind [replicate 5, (:[])] [1,2,3] == [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,1,2,3])
    ]

runTests :: [(String, Bool)] -> IO ()
runTests at = mapM_ (\(a,b) -> catch (b `seq` pure b) hError >>= \bv -> if bv then pure () else fail ("Test Failed: \x1b[31m" ++ a ++ "\x1b[0m")) at *> (putStrLn $ "\x1b[32mAll " ++ show (length at) ++ " tests passed\x1b[0m")
    where
        hError :: ErrorCall -> IO Bool
        hError _ = pure False