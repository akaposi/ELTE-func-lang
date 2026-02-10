{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy01 where

{-

Topics:
- The longer version is available on GitHub
- Requirements:
  - Weekly homework assignments, each with a 2-week deadline, mandatory
  - 3 major assignments during the semester (3 * 4 points), not mandatory. If you earn 8 points, you will receive +1 grade on the exam if you have a 2.
  - During the exam period, there will be an exam covering the entire semester's practical material.
  - Lecture: Tuesday 10:00
  - You may miss a maximum of 3 practical classes

- Any IDE and software may be used for the course (VSCode, Emacs, Neovim, etc.), including the Haskell Language Server
- Anything may be used during the exam, except for human assistance and AI

- Class files: https://github.com/Akaposi/ELTE-func-lang/tree/master/2025-26-2/gyakX
- GyXX_pre.hs = File before class
- GyXX.hs     = File after class

- The course is based on the Functional Programming (IP-18FUNPEG) course
- For those who are behind: lambda.inf.elte.hu

GHCi reminder:
- :l <file>     - loads the file into GHCi
- :r            - reloads the loaded files
- :bro <module> - short for browse, prints the contents of a module
- :t <expr>     - prints the type of an expression
- :i <that>     - prints information about an fv/type/etc. (binding strength, where it is defined, etc.)
- :set <flag>   - turns on a flag (e.g., -Wincomplete-patterns)
- :q            - exit

Pragmas:
{-# <PRAGMA> <OPTIONS> #-}
- This always goes at the top of the file
- Important pragmas:
  - OPTIONS_GHC: turns on GHC flags, e.g. -Wincomplete-patterns, which gives a warning if a pattern matching is not total
  - LANGUAGE: Turns on language extensions, e.g. InstanceSigs, which allows explicit typing of instance functions

-}


-- Today's topic: Recalling functional programing (functions, pattern matching, algebraic data types, type systems)
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True 
xor False True = True
xor False False = False

xor' :: Bool -> Bool -> Bool
xor' x y = case x of
  True -> not y
  False -> y

-- There may be multiple solutions (pattern matching, builtin functions)
-- New "case" expression
{-
case x of
  True -> ...
  False -> ...
-}

-- Let/Where expression: local definitions:
twelve :: Int
twelve = x + x
  where
    x = 6

twelve' :: Int
twelve' = let x = 6 in x + x

-- Polymorphism: The function works for any type arguments, as long as its consistent throughout the function
id' :: a -> a
id' x = x

-- There can be multiple type variables
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (x, (y, (z, w))) = (y, z)

-- Help: Hole technology!
-- In Haskell, if we write _ on the right side of the equality, the compiler tells us what type of expression is needed there

-- There are multiple solutions for every function (e.g., with built-in functions)

f2 :: (a -> b) -> a -> b
f2 = id'
--f2 f = f
--f2 f x = f x

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g = f . g 
--f3 f g x = f (g x)

f4 :: (a -> (b -> c)) -> (b -> (a -> c))
f4 f x y = f y x

-- Helper functions:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

-- a -> (b -> c)
-- (a, b) -> c
f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- a -> b -> c == a -> (b -> c) due to Currying
f5 f a b = f (a, b)

f6 :: (a -> (b -> c)) -> ((a, b) -> c)
f6 f x = f (fst x) (snd x)
--f6 f (a, b) = f a b

-- If the solution requires you to supply a function, use lambdas
-- eg.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a) , \a -> snd (f a))

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) x = (f x, g x)

-- ADT reminder:
-- Either data type. It has two constructors, Left and Right, which store either a or b:
{-
:i Either
data Either a b = Left a | Right b
-}

f9 :: Either a b -> Either b a
f9 (Left x) = Right x
f9 (Right y) = Left y

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\a -> f (Left a), \b -> f (Right b))

f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f, g) (Left a) = f a
f11 (f, g) (Right b) = g b

-- Bonus

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 = undefined

f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 = undefined

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 = undefined

-- List reminder
-- How is the list defined?
-- data List a = [] | a : [a]

-- Define the map and filter functions with a list generator, recursion, and folding.
-- [1,2,3,4] = (1 : (2 : (3 : (4 : []))))

-- recursion
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (a : as) = (f a : map' f as)

-- list comprehension (list generator)
map'' :: (a -> b) -> [a] -> [b]
map'' f as = [ f a | a <- as ]

-- folds
map''' :: (a -> b) -> [a] -> [b]
map''' f as = foldr (\a bs -> f a : bs) [] as
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- foldr (+) 5 [1,2,3,4] 
-- = foldr (+) 5 (1 : (2 : (3 : (4 : []))))
-- = 1 + foldr (+) 5 (2 : (3 : (4 : [])))
-- = 1 + (2 + foldr (+) 5 (3 : (4 : [])))
-- = 1 + (2 + (3 + foldr (+) 5 (4 :: [])))
-- = 1 + (2 + (3 + (4 + foldr (+) 5 [])))
-- = 1 + (2 + (3 + (4 + 5))) = 15
-- Note: I wrote this wrong during the practice class, apologies!!

-- recursion
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (a : as) = 
  if (f a) then (a : filter' f as) else (filter' f as)

-- homework: define filter'' and filter''' using list comprehension and foldr

-- Let's define some other useful list functions that are part of the standard library.
-- !! It is not recommended to reinvent the entire Haskell stdlib for the exam !!

take', drop' :: Int -> [a] -> [a]

take' n [] = []
take' n (x : xs)  
  | n <= 0    = []
  | otherwise = x : take' (n-1) xs

drop' n [] = []
drop' n (x : xs) 
  | n <= 0    = (x : xs)
  | otherwise = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' = undefined

takeWhile', dropWhile' :: (a -> Bool) -> [a] -> [a]

takeWhile' = undefined
dropWhile' = undefined

span', partition' :: (a -> Bool) -> [a] -> ([a], [a])

span' = undefined
partition' = undefined

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined

cycle' :: [a] -> [a]
cycle' = undefined

iterate' :: a -> (a -> a) -> [a]
iterate' = undefined

repeat' :: a -> [a]
repeat' = undefined

replicate' :: Int -> a -> [a]
replicate' = undefined

nub' :: Eq a => [a] -> [a]
nub' = undefined

