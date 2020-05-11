{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Practice7 where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as S

newtype Writer w a = Writer { unWriter :: State w a }
  deriving (Functor, Applicative, Monad)

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter (Writer sm) = runState sm mempty

tell :: Semigroup w => w -> Writer w ()
tell w = Writer $ do
  s <- get
  put (s <> w)

sumAndLog :: [Int] -> Writer [String] Int
sumAndLog [] = return 0
sumAndLog (x:xs) = do
  if odd x then
    tell $ [show x ++ " is odd"]
  else
    tell $ [show x ++ " is even"]
  res <- sumAndLog xs
  return $ x + res

uniquesM :: Ord a => [a] -> Writer (Set a) ()
uniquesM [] = return ()
uniquesM (x:xs) = do
  tell (S.singleton x)
  uniquesM xs
