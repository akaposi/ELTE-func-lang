import Data.Char
import Data.List
import Control.Monad

{- List monad -}

-- Consider the following interpretation of the Maybe monad's bind operator:
-- "Continue the computation with all the possible values from the previous result!"
-- In case the result is contained in a Just constructor, the computation can
-- continue, but if Nothing is returned, then there's nothing to compute with.
--
-- If we generalize this from zero or one result to zero or more results, we
-- get the behavior of the List monad. It continues the computation on all the
-- avaliable values and merges the different results into a single list.


{- Example -}
-- Let's implement a simple algorithm that generates possible passwords, which
-- fulfill certain criteria!

-- Let's suppose that there are three possible casings of parts,
-- lowercase, UPPERCASE and CapitalCase.
lowercase :: String -> String
lowercase = map Data.Char.toLower

uppercase :: String -> String
uppercase = map Data.Char.toUpper

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = Data.Char.toUpper c : map Data.Char.toLower cs

-- With the `casing` function we can generate all the possibilities for one string.
casing :: String -> [String]
casing s =
  [ lowercase s
  , uppercase s
  , capitalize s
  ]


-- If passwords are made of three parts, a color, an animal and a number, we can
-- get the possible combinations (including casing) in the following way:
--
-- Notice that there is also a `guard`, which acts a filter by returning an
-- empty list if the predicate is false, so the computation does not continue
-- on that branch, but returning a list with a single element if the condition
-- is fulfilled, thus enabling the computation to progress without further
-- branching.
-- This way we can express constraints about the branches we want to visit.
passwords :: [String]
passwords = do
  color <- ["red", "yellow", "green", "turquoise", "blue"]
  casedColor <- casing color

  animal <- ["cat", "giraffe", "zebra"]
  casedAnimal <- casing animal

  number <- [1..3]

  let pw = casedColor ++ casedAnimal ++ show number
  guard (length pw > 12)
  return pw


{- Task -}
-- Using this, solve the following problem of generating all the possible cells
-- a knight can reach on a chessboard from a given starting position in a given
-- number of moves:

type Position = (Char, Int)

{-
    -----------------
  8 | | | | | | | | |
    -----------------
  7 | | | | | | | | |
    -----------------
  6 | | | | | | | | |
    -----------------
  5 | | | | | | | | |
    -----------------
  4 | | | | | | | | |
    -----------------
  3 | | | | | | | | |
    -----------------
  2 | | | | | | | | |
    -----------------
  1 | |x| | | | | | |
    -----------------
     a b c d e f g h
-}


-- Transpose a character in the alphabet
-- Examples:
--   + transposeChar   3  'a' == 'd'
--   + transposeChar (-2) 'h' == 'f'
-- Hint: Make use of the fact that the `Char` type has an instance for the
--       `Enum` typeclass, which makes it convertible from and to `Int`
--       using the `fromEnum` and `toEnum` functions.
transposeChar :: Int -> Char -> Char
transposeChar = undefined

-- Check if a position is valid on the board!
-- Examples:
--   + isValidOnBoard ('b', 1) == True
--   + isValidOnBoard ('i', 9) == False
-- Hint: Make use of the `elem` function and list range expressions!
isValidOnBoard :: Position -> Bool
isValidOnBoard = undefined

-- List all the possible relative moves for a knight!
-- [(-2, -1), (-2, 1), ..., (2, 1)]
possibleKnightMoves :: [(Int, Int)]
possibleKnightMoves = undefined

-- List all the legal moves for a knight from a certain position!
-- Examples:
--   + knightMove ('b', 1) == [('a', 3), ('c', 3), ('d', 2)]
knightMove :: Position -> [Position]
knightMove = undefined

-- List all the positions a knight can land on after two moves starting from B1!
positionsInTwoMovesFromB1 :: [Position]
positionsInTwoMovesFromB1 = undefined

tests :: [Bool]
tests = [ transposeChar   3  'a' == 'd'
        , transposeChar (-2) 'h' == 'f'
        , isValidOnBoard ('b', 1) == True
        , isValidOnBoard ('i', 9) == False
        , elem ('b', 1) positionsInTwoMovesFromB1
        , elem ('b', 5) positionsInTwoMovesFromB1
        , elem ('c', 2) positionsInTwoMovesFromB1
        , elem ('c', 4) positionsInTwoMovesFromB1
        , elem ('a', 2) positionsInTwoMovesFromB1
        , elem ('a', 4) positionsInTwoMovesFromB1
        , elem ('b', 1) positionsInTwoMovesFromB1
        , elem ('b', 5) positionsInTwoMovesFromB1
        , elem ('d', 1) positionsInTwoMovesFromB1
        , elem ('d', 5) positionsInTwoMovesFromB1
        , elem ('e', 2) positionsInTwoMovesFromB1
        , elem ('e', 4) positionsInTwoMovesFromB1
        , elem ('b', 1) positionsInTwoMovesFromB1
        , elem ('b', 3) positionsInTwoMovesFromB1
        , elem ('c', 4) positionsInTwoMovesFromB1
        , elem ('e', 4) positionsInTwoMovesFromB1
        , elem ('f', 1) positionsInTwoMovesFromB1
        , elem ('f', 3) positionsInTwoMovesFromB1
        , not (elem ('f', 5) positionsInTwoMovesFromB1)
        ]
