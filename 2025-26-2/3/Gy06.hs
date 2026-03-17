{-# LANGUAGE DerivingVia, DerivingStrategies #-}

module Gy06 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Semigroup
import Data.List

-- Let's say we are system administrators and we want to implement a permission system in Haskell.
-- To do this, we want to determine which home directory belongs to each user.

type User = String
data UserInfo = MkUserInfo { getHomeDirectory :: String, isAdmin :: Bool } deriving (Eq, Show)
type Environment = [(User, UserInfo)] -- a map of names to home directories and admin status

-- The environment does not change during operations, so no output environment is necessary
-- Let's define a function that retrieves the home directory of a given username

homeDirOf :: String -> Environment -> Maybe String
homeDirOf uname env = case lookup uname env of
                      Nothing -> Nothing
                      Just inf -> Just (getHomeDirectory inf)

-- Define a function that retrieves the home directories of all admin users
-- For demonstration purposes, do not use list functions and do not write helper functions
-- During recursive calls, retrieve elements from the environment because, since there is no output, this is *not reflected in the result*

getAdminHomes :: Environment -> [String]
getAdminHomes [] = []
getAdminHomes ((name, info):xs)
  | isAdmin info = getHomeDirectory info : getAdminHomes xs
  | otherwise = getAdminHomes xs

-- In the above examples, the transfer of the environment was explicit (we had to do it manually).
-- However, with an abstraction layer, it can be made implicit:
{-
newtype Reader r a = Reader { runReader :: r -> a }
-}

-- A Reader monádot fog alkotni, tehát használható a bind művelet
-- (>>=) :: Reader a  -> (a -> Reader b) -> Reader b
-- (>>=) :: (r ->  a) -> (a -> r ->   b) -> r ->   b
{-           ^     |      ^    ^      |     |      ^
             |     \------/    |      \-----+------/
             \-----------------+------------/

Duplication and transmission of the r environment is now implicit
Similar to State, there is a reader function
-}

homeDirOfR :: String -> Reader Environment (Maybe String)
homeDirOfR s = reader (homeDirOf s)

getAdminHomesR :: Reader Environment [String]
getAdminHomesR = reader getAdminHomes

-- The Reader monad has the following two operations
-- ask: queries the environment
-- local: changes it locally

ask' :: Reader r r
ask' = reader (\r -> r) -- id

local' :: (r -> r) -> Reader r a -> Reader r a
local' f m = reader (\r -> runReader m (f r))

-- Define the two functions above using ask and local

homeDirOfRM :: String -> Reader Environment (Maybe String)
homeDirOfRM uname = do
  env <- ask
  case lookup uname env of
    Nothing -> return Nothing
    Just inf -> return $ Just (getHomeDirectory inf)

getAdminHomesRM :: Reader Environment [String]
getAdminHomesRM = ask >>= \env -> case env of
                                    [] -> return []
                                    ((uname, uinfo):_) -> if isAdmin uinfo then rest 
                                      >>= (\ls -> return $ getHomeDirectory uinfo : ls)
                                      else rest >>= return
                        where
                          rest = local (drop 1) getAdminHomesRM

getAdminHomesRM' :: Reader Environment [String]
getAdminHomesRM' = do
  env <- ask
  case env of
    [] -> return []
    ((uname, uinfo):xs) -> if isAdmin uinfo 
      then do
        rest <- local (const xs) getAdminHomesRM'
        return (getHomeDirectory uinfo : rest)
      else local (const xs) getAdminHomesRM'


{-
const xs = \_ -> xs
-}
-- Extra tasks

-- Define the labelWith function with reader monad

labelListR :: Num i => [a] -> Reader i [(i,a)]
labelListR (x : xs) = ask >>= \i -> fmap ((i,x) :) $ local (+1) (labelListR xs)
labelListR [] = return []

labelListR' :: Num i => [a] -> Reader i [(i,a)]
labelListR' (x : xs) = do
  i <- ask
  rest <- local (+1) (labelListR xs)
  return ((i,x) : rest)
labelListR' [] = return []

-- Define the sum function in a tail recursive way, such that the acummulator is in the reader environment

sumTRR :: Num a => [a] -> Reader a a
sumTRR = undefined

-- Define the filterWithIndex function, which also filters based on index

filterWithIndexR :: Num i => (i -> a -> Bool) -> [a] -> Reader i [a]
filterWithIndexR = undefined

-- Define the foldl function using the reader monad

foldlR :: (b -> a -> b) -> [a] -> Reader b b
foldlR = undefined

-- WRITER

-- Let's say we are QA developers and we want to test the code coverage of our code.
-- We track every dependency with a UUID.
type UUID = String

-- We change our arithmetic functions to collect the function calls that were made in the output
-- If the API request is "localhost," we return True, otherwise False. Regardless of this, we log the "apirequest" UUID
apiRequest :: String -> (Bool, [UUID])
apiRequest "localhost" = (True, ["localhost"]) 
apiRequest x = (False, [x]) 

-- Define the following function, which sends an API request to the address "1.0.0.1" if it receives an even number as a parameter, and does nothing if it does not.
-- Regardless of this, log the "nameserver" UUID.
-- Return half of the number as the result.
nameserver :: Int -> (Int, [UUID])
nameserver x
  | even x = let (_,l) = apiRequest "1.0.0.1" in (div x 2, "nameserver" : l)
  | otherwise = (div x 2, ["nameserver"])

-- In the above two functions, the second parameter of the tuple, [UUID], can be abstracted away.
-- This is the Writer monad.
{-
newtype Writer w a = Writer { runWriter :: (w,a) }
-}
-- A writer monádot fog alkotni, ha a w egy monoid
-- (>>=) :: Writer w a  -> (a -> Writer w b)  -> Writer w b
-- (>>=) ::       (w,a) -> (a ->       (w,b)) -> (w,b)
{-                 | \------^           | |       ^ ^
                   |                    | \-------+-/
                   \-------------------<+>--------/

Since the two w's must be combined in some <+> way, the Semigroup constraint is necessary. The Monoid constraint is necessary because of the return:
return :: a -> Writer w a
return :: a -> (w,a)
-}

-- Similar to the previous monads, there is a writer function

apiRequestW :: String -> Writer [UUID] Bool
apiRequestW s = writer (apiRequest s)

nameserverW :: Int -> Writer [UUID] Int
nameserverW x = writer (nameserver x)

-- The writer has a primitive operation that is relevant to us:

tell' :: Monoid w => w -> Writer w ()
tell' w = writer ((),w)

-- For more advanced usage, see listen and pass
-- Define the above function using tell

apiRequestWD :: String -> Writer [UUID] Bool
apiRequestWD "localhost" = do 
  tell ["localhost"]
  return True
apiRequestWD x = do
  tell [x]
  return False

nameserverWD :: Int -> Writer [UUID] Int
nameserverWD x
  | even x = do
    apiRequestWD "1.0.0.1"
    tell ["nameserver"]
    return (div x 2)
  | otherwise = do
    tell ["nameserver"]
    return (div x 2)

-- Most often, the writer environment will be a list, but it can be any Monoid
-- Let's assume that we define a function and define the following Hash type for it

newtype Hash = Hash { unHash :: Integer }
  deriving newtype (Show, Eq, Ord, Num, Real, Integral, Enum) -- Inherits instances from Integer
  deriving Semigroup via Product Integer                      -- Semigroup and Monoid instance based on multiplication
  deriving Monoid    via Product Integer                      

-- The function will now multiply the numbers without a modulus (not very effective, don't use it in production)

-- Sieve of Erasthothenes
primes :: Integral a => [a]
primes = unfoldr (\(x:xs) -> Just (x, filter (\y -> mod y x /= 0) xs)) [2..]

-- Lets hash an integer list the following way:
-- For each element we index into the list of primes and multiply it into the accumulating hash
hashIntList :: [Int] -> Writer Hash ()
hashIntList = undefined

-- Repeat the same with an arbitrary foldable
hashFoldable :: Foldable f => f Int -> Writer Hash ()
hashFoldable = undefined
