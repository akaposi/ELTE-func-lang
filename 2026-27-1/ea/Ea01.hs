import Data.List
{-
Funkcionális Nyelvek (Advanced Haskell)
14:15-15:00 15:05-15:50

Szumi Xie
szumi@inf.elte.hu

ask questions whenever


Haskell:
  (compiled/interpreted)
  statically-typed lazy pure functional programming language

- programming language:
  - language to communicate with computers
  - computers execute
  - formal language:
    - strict rules
    - unambiguous meaning
- functional:
  - it has higher-order functions:
    - functions as inputs
      map :: (a -> b) -> [a] -> [b]
  - functions are first-class values
    - functions as outputs
      (+) :: Int -> (Int -> Int)
    - functions in data structures
      [Int -> Int]
  - advantage:
    map (+ 1)
    - in a language without higher-order functions
      mapPlusOne :: [Int] -> [Int]
      mapPlusOne [] = []
      mapPlusOne (x:xs) = x + 1 : mapPlusOne xs
    - no duplication
- pure:
  - no side-effects
  - functions are actually functions (in maths)
    f :: Int -> Int
    f 1 = 2
    f 2 = 3
    - applying functions to the same arguments will always result in same output
  - in non-pure languages (Python):
    x = 1
    def f(n):
      global x
      x = x + 1
      return n + x
    f(2) => 4
    f(2) => 5
  - not completely:
    - (IO monad)
    - exceptions
    - non-termination
      f :: Int -> Int
      f 1 = 2
      f 2 = 3
      f 3 = error "error"
      f 4 = f 4
  - advantages:
    - no accidental state change
    - better refactoring
      (x * x) + (y * y)
      <-->
      square x = x * x
      square x + square y
    - parallelization
- statically-typed:
  - at compile time, program needs to be well-typed, checked by type checker
  - everything has a type
  - rules for well-typedness
    f :: a -> b
    x :: a
    ---
    f x :: b
  - advantages:
    - prevents mistakes
    - easier for modelling
- lazy:
  - only computes value when needed
    - const x y = x
      const 5 (expensive ()) = 5
    - infinite lists
      take 10 [0..]
  - advantages:
    - recursion without needing tail-call elimination
    - modularity
      primes :: [Int]
      take 100 primes
      primes !! 100
-}

primes :: [Int]
primes = sieve [2..]
  where
    sieve :: [Int] -> [Int]
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- primes with difference of 2
twinprimes :: [(Int, Int)]
twinprimes = filter (\(x, y) -> y - x == 2) $ zip primes (tail primes)

square :: Int -> Int
square x = x * x
{-
evaluation strategies:
- call-by-value (strict)
  square (1 + 2) =
  square 3 =
  3 * 3 =
  9
- call-by-name
  square (1 + 2) =
  (1 + 2) * (1 + 2) =
  3 * (1 + 2) =
  3 * 3 =
  9
- call-by-need (Haskell) - sharing
  square (1 + 2) =
  (let x = 1 + 2 in square x) =
  (let x = 1 + 2 in x * x) =
  (let x = 3 in x * x) =
  3 * 3 =
  9

-}

f :: Int -> [Int]
f x = [x, x]

loop :: a
loop = loop

g :: Int -> [Int]
g x = [x + 1, x] -- (x + 1) : x : []

data Nat = Zero | Suc Nat
  deriving (Show)

one :: Nat
one = Suc Zero

infinity :: Nat
infinity = Suc infinity

-- arguments to constructors are stored in a thunk

h :: Maybe Int -> [Int]
h Nothing = []
h (Just x)
  | x == 0 = [x, x]
  | otherwise = [x, x, x]
-- length (h loop) -> loops

{-
evaluation is caused by pattern matching or primitive operations (+,-,*,==,...)
-}

{-
repeat :: a -> [a]
repeat x = x : repeat x

take :: Int -> [a] -> [a]
take n xs | n == 0 = []
take n [] = []
take n (x:xs) = x : take (n - 1) xs

take 4 (repeat 0) =
take 4 (0 : repeat 0) =
0 : take (4 - 1) (repeat 0) =
0 : take 3 (repeat 0) =
0 : take 3 (0 : repeat 0) =
0 : 0 : take (3 - 1) (repeat 0) =
0 : 0 : take 2 (repeat 0) =
0 : 0 : take 2 (0 : repeat 0) =
0 : 0 : 0 : take (2 - 1) (repeat 0) =
0 : 0 : 0 : take 1 (repeat 0) =
0 : 0 : 0 : take 1 (0 : repeat 0) =
0 : 0 : 0 : 0 : take (1 - 1) (repeat 0) =
0 : 0 : 0 : 0 : []

-- ^ equational reasoning
-}

-- 0-1 knapsack problem - dynamic programming

type Weight = Int
type Value = Double

{-
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-}

knapsack :: [(Weight, Value)] -> Weight -> Value
knapsack items maxW = helper (length items) maxW
  where
    helper i w
      | i == 0 = 0
      | itemW > w = helper (i - 1) w
      | otherwise =
        max
          (helper (i - 1) w)
          (helper (i - 1) (w - itemW) + itemV)
      where
        (itemW, itemV) = items !! (i - 1)

example :: [(Weight, Value)]
example = [(2, 3), (3, 5), (5, 11), (7, 9), (8, 12)]

example2 :: [(Weight, Value)]
example2 =
  replicate 10 (2, 3) ++
  replicate 15 (3, 5) ++
  replicate 7 (5, 8)

knapsack' :: [(Weight, Value)] -> Weight -> Value
knapsack' items maxW = m !! length items !! maxW
  where
    m :: [[Value]]
    m =
      [0 | _ <- [0..maxW]] :
      [ [ if itemW > w
            then m !! (i - 1) !! w
            else
              max
                (m !! (i - 1) !! w)
                (m !! (i - 1) !! (w - itemW) + itemV)
        | w <- [0..maxW]]
      | (i, (itemW, itemV)) <- zip [1..] items]
    -- tying the knot
