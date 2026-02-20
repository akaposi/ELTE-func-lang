-- Funkciónális Nyelvek (in English)
-- Functional languages = Advance Haskell
-- Functional programming = Basic Haskell

{-
Szumi Xie   szumi@inf.elte.hu

Tuesday 10:15-11:00 11:05-11:50

ask questions whenever

Haskell: statically-typed lazy pure functional programming language

- programming language:
  - formal language: formal rules        not natural language
  - rules for how it computes
- functional programming language:
  - first-class functions: functions can be used values
    - example: map: takes a function
      map (\x -> x + 1)
      instead of
      mapPlusOne [] = []
      mapPlusOne (x:xs) = x + 1 : mapPlusOne xs
  - what are functions?
    - in math: given an input, there is an output
      - two equal inputs give equal outputs
        f(2) == 3
        f(2) == 3
    - in imperative programming: rnd: random number generator
      - x = 0
        def f(n):
          global x   -- maybe needed
          x = x + 1
          return n + x
        f(2) == 3
        f(2) == 4
- pure: no side effects
  - functions are like in functions in math
  - but Haskell has some side effect:
    - x = error "oops"
    - x = x
    - x = unsafePerformIO ...
  - pros:
    - easy parallelism
    - referential transparency:
      - never need to know whether a variable is a reference or the actual value
      - refactor easily: always replace something with its definition
- lazy: only evaluates when needed
  - not strict: f(x + 1), then x + 1 is immediately computed
  - fives = 5 : fives
  - take 10 fives
  - loop () = loop ()
  - const x y = x
  - call-by-value: strict
  - call-by-name vs call-by-need
  - Haskell is call-by-need
  - powerOfTwo 0 = 1
  - powerOfTwo n = let x = powerOfTwo (n - 1) in x + x
-}
powerOfTwo :: Integer -> Integer
powerOfTwo 0 = 1
powerOfTwo n = let x = powerOfTwo (n - 1) in x + x

{-
call-by-name:
powerOfTwo 2   =
let x = powerOfTwo 1 in x + x   =
powerOfTwo 1 + powerOfTwo 1     =
(powerOfTwo 0 + powerOfTwo 0) + (powerOfTwo 0 + powerOfTwo 0)
(1 + 1) + (1 + 1)
2 + 2 -- calculates 1 + 1 twice
4

call-by-need:
powerOfTwo 2   =
let x = powerOfTwo 1 in x + x   =
let x = (let y = powerOfTwo 0 in y + y) in x + x   =
let x = (let y = 1 in y + y) in x + x   =
let x = 1 + 1 in x + x   =
let x = 2 in x + x   =
2 + 2
4

statically-typed: vs dynamically-typed
- everything has a type
- compiler needs to check everything is well-typed
  - f : Int -> String
  - x : Int
  - f x : String

- Haskell has type inference
-}

plusOne x = x + 1

-- if True then 1 else plusOne "hello"

-- we continue at 11:04

-- Int, Integer, Char, Bool
-- [], ->

data Nat = Zero | Succ Nat -- successor
  deriving (Show)

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

infinity :: Nat
infinity = Succ infinity

isZero :: Nat -> Bool
isZero Zero = True
isZero (Succ n) = False

instance Eq Nat where
  -- Nat -> Nat -> Bool
  Zero == Zero = True
  Succ m == Succ n = m == n
  _ == _ = False

instance Ord Nat where
  Zero <= _ = True
  Succ _ <= Zero = False
  Succ m <= Succ n = m <= n

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
-- add two two =
-- add (Succ (Succ Zero)) (Succ (Succ Zero)) =
-- Succ (add (Succ Zero) (Succ (Succ Zero))) =
-- Succ (Succ (add Zero (Succ (Succ Zero)))) =
-- Succ (Succ (Succ (Succ Zero))))

-- length :: [a] -> Int
goodLength :: [a] -> Nat
goodLength [] = Zero
goodLength (_:xs) = Succ (goodLength xs)

-- isPrefixOf [1, 2] [1, 2, 3, 4] == True
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (x:xs) [] = False
isPrefixOf (x:xs) (y:ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False


-- suffixes [1, 2, 3] == [[1, 2, 3], [2, 3], [3], []]
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs


-- isInfixOf [2, 3] [1, 2, 3, 4] == True
-- isInfixOf [3, 2] [1, 2, 3, 4] == False
-- isInfixOf [1, 2] [1, 2, 3, 4] == True
-- isInfixOf [3, 4] [1, 2, 3, 4] == True
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys = any (isPrefixOf xs) (suffixes ys)

-- isInfixOf [97..99] [1..100]
-- suffixes [1..100] = [[1,2,3,...100], [2,3,4,...100], ..., [100], []]

-- laziness allow us to compose
-- without generating all the structure
