module Gy06Gyak where

import Control.Monad.Reader

factM :: (MonadReader Int m) => m Int
factM = do
    ind <- ask
    if ind <= 0 then pure 1
    else do
        re <- local (+ (-1)) factM
        pure $ ind * re

factMT :: (Monad m) => ReaderT Int m Int
factMT = factM

-- newType Identity a = Id {id :: a}

data List a = Nil | Cons {item :: a, predl :: List a}

k = Cons 10 (Cons 8 Nil)

-- >>> item k
-- 10
-- >>> item $ predl k
-- 8
-- >>> item $ k { item = 7}
-- 7
-- >>> item $ k {predl = Nil}
-- 10

factR :: Reader Int Int
factR = factM

--- n!/(k!* (n-k)!)
nCr :: (MonadReader Int n , MonadReader Int k) => n ( k Int )
nCr = do
    nf <- factM
    n <- ask
    pure $ do
        kf <- factM
        nmkf <- local (n -) factM
        pure $ nf `div` (kf * nmkf)

nCr' :: (MonadReader Int n , MonadReader Int k) => n ( k Int )
nCr' = factM >>= \nf -> ask >>= \n -> pure $ factM >>= \kf -> local (n -) factM >>= \nmkf -> pure $ div nf $ (kf *) $ nmkf

i :: IO ()
i = do
    do
        do
            do
                do
                    do
                        do
                            do
                                do
                                    do
                                        do
                                            do
                                                do
                                                    do
                                                        do
                                                            do
                                                                do
                                                                    do
                                                                        do
                                                                            do
                                                                                do
                                                                                    do
                                                                                        do
                                                                                            do
                                                                                                do
                                                                                                    undefined

{-

plus :: Int -> Int -> Int
plus a b = a + b

k :: Int
k = plus 10 5

plus' :: {Int} -> Int -> Int
plus' {a} b = a + b

1. opció (buta nyelv)    : k' :: {Int} -> Int
2. opció (okosabb nyelv) : k' :: Int
k' = plus' 5

k'' :: Int
k'' = plus' {10} 5

-}

-- >>> runReader factM 5
-- 120

{-
newType = Új típus, egy konstruktorral, aminek EGY paramétere van.
-}



