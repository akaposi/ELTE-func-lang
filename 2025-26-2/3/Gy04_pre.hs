{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gyak04 where
import Control.Monad

-- Problem:
-- Suppose that we have lots of functions resulting in Maybe a for example:

incrementIfEven :: Integral a => a -> Maybe a
incrementIfEven x
  | even x = Just (x + 1)
  | otherwise = Nothing

combineThrees :: Integral a => (a -> a -> a) -> a -> a -> Maybe a
combineThrees f x y
  | (x + y) `mod` 3 == 0 = Just (f x y)
  | otherwise = Nothing

-- How could we implement a function that takes a number as a parameter, calls incrementIfEven on it
-- then if that returns some number wrapped in Just, it calls combineThrees with the original argument and the result,
-- along with the multiplication function, then calls incrementIfEven again if possible?
-- E.g.:
-- magicFunction 4 == Just 21 (incrementIfEven 4 == 5, 4 + 5 `mod` 3 == 0, 4 * 5 == 20, incrementIfEven 20 == 21)
-- magicFunction 3 == Nothing (incrementIfEven 3 == Nothing)
-- magicFunction 2 == Nothing (incrementIfEven 2 == 3, 2 + 3 `mod` 3 /= 0)

magicFunction :: Integral a => a -> Maybe a
magicFunction = undefined

-- This isn't such a big deal for a single Maybe test, but if you need to do a lot of them, it can introduce quite a bit of boilerplate code
-- the so-called "side effect" (i.e., if a calculation does something else besides returning a result, in the case of Maybe, the operation may fail).
-- The solution to this is the Monad type class.
{-
:i Monad
type Monad :: (* -> *) -> Constraint
class Functor m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  {-# MINIMAL (>>=), return #-}
-}
-- The >>= (bind) operation models the use of the result of a previous "effectful" calculation.
-- In the case of Maybe (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--                                            ^ only runs if the first parameter is Just a

-- Bind, very similar to the dependency application ($)
-- (>>=) :: m a -> (a -> m b) -> m b
-- ($)   ::   a -> (a ->   b) ->   b
-- Explain why

magicFunctionM :: Integral a => a -> Maybe a
magicFunctionM x = undefined

-- This allows you to compose multiple operations that have side effects.
-- If you don't like the >>= notation, there is also an imperative-style notation.
{-
do
   x <- y
   a
===
y >>= \x -> a
-}

magicFunctionDo :: Integral a => a -> Maybe a
magicFunctionDo = undefined


-- Monad example: IO monad
-- "IO a" means a value of type "a" that requires some I/O operation to be performed, e.g., reading from the console
{-
getLine :: IO String
putStrLn :: String -> IO ()
                         ^ The equivalent of 'void' in imperative languages, a type that has exactly one irrelevant element
readLn :: Read a => IO a
print :: Show a => a -> IO ()
-}


-- In the case of IO, >>= must be thought of in abstract terms
-- IO a    = A value of type 'a' whose value is the result of an I/O calculation
-- f >>= g = Calculates the f IO operation and passes the result to g
-- In practice, this is not what happens, so there is no "IO a -> a" operation
-- >>= always keeps values in an IO environment: You can't get rid of monads
-- This guarantees that there are no IO operations in a pure environment


-- The >> operation is useful for operations that return unit () (called unit)
-- m1 >> m2 = m1 >>= \_ -> m2
--                    ^ result is irrelevant, just run it

-- Write IO operations using do-notation and binds that
-- a) read two lines and print their concatenation
-- b) read a number and print its square
-- c) print the elements of a given list, one element per line
-- d) for each element in a given list, read an element and add it to the respective element in the list

readAndConcat :: IO ()
readAndConcat = undefined

readAndConcat' :: IO ()
readAndConcat' = undefined

readAndSq :: IO ()
readAndSq = undefined

readAndSq' :: IO ()
readAndSq' = undefined

printAll :: Show a => [a] -> IO ()
printAll = undefined

printAll' :: Show a => [a] -> IO ()
printAll' = undefined

readAndAdd :: (Read a, Num a) => [a] -> IO [a]
readAndAdd = undefined

readAndAdd' :: (Read a, Num a) => [a] -> IO [a]
readAndAdd' = undefined

-- What else is a Monad?
-- Lists for example:
{-

[1,2,3] >>= \n -> replicate n n

         >>=  
[
1        ->         [1]                  \ 
2        ->         [2,2]                | => [1,2,2,3,3,3]
3        ->         [3,3,3]              /
]

-}

-- Another approach: Join

join' :: Monad m => m (m a) -> m a
join' = undefined
