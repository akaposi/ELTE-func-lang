{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Data.Char
import Data.List
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.State

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Functor, Foldable, Traversable)

instance Show a => Show (Tree a) where
  show (Leaf x) = "(" ++ show x ++ ")"
  show (Node l r) = "[" ++ (show l) ++ " + " ++ (show r) ++ "]"

-- instance Traversable Tree where
--   traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
--   traverse f (Leaf a)   = Leaf <$> f a
--   traverse f (Node l r) = Node <$> (traverse f l) <*> (traverse f r)

t :: Tree Int
t = Node (Node (Leaf 5) (Node (Leaf 7) (Leaf (-12)))) (Node (Leaf 3) (Leaf (-8)))

t' :: Tree Int
t' = Node (Node (Leaf (-3)) (Leaf (-7))) (Node (Leaf 19) (Leaf 2))

-- Rotate the signs of numbers in a tree.
rotateSigns :: Tree Int -> Tree Int
rotateSigns t = evalState (traverse go t) (undefined) where
  go :: Int -> State Int Int
  go i = do
    undefined

-- Collect the sum of the negative and positive numbers
-- from a tree separately into a pair.
collectWithSigns :: Tree Int -> (Int, Int)
collectWithSigns t = execState (traverse go t) (undefined) where
  go :: Int -> State (Int, Int) ()
  go i = do
    undefined

{- List monad -}

{- Example -}
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

passwords :: [String]
passwords = do
  undefined
  -- color <- ["red", "yellow", "green", "turquoise", "blue"]
  -- casedColor <- casing color
  --
  -- animal <- ["cat", "giraffe", "zebra"]
  -- casedAnimal <- casing animal
  --
  -- number <- [1..3]
  --
  -- let pw = casedColor ++ casedAnimal ++ show number
  -- guard (length pw > 12)
  -- return pw


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
possibleKnightMoves = do
  undefined

-- List all the legal moves for a knight from a certain position!
-- Examples:
--   + knightMove ('b', 1) == [('a', 3), ('c', 3), ('d', 2)]
knightMove :: Position -> [Position]
knightMove = do
  undefined

-- List all the positions a knight can land on after two moves starting from B1!
positionsInTwoMovesFromB1 :: [Position]
positionsInTwoMovesFromB1 = do
  undefined



{- Parser -}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- (State String + Maybe) monad
-- Parser a : function, which can modify a String state and can potentially fail
--   Nothing     : parse error
--   Just (a, s) : successful parsing with result and remaining input

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- No input is consumed
  return a = Parser $ \s -> Just (a, s)
  -- runParser (return 10) "asdf" == Just (10,"asdf")

  -- Executing parsers sequentially
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  -- Parser that fails instantly
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- Try the first parser, in case of failure try the second one
  -- (p1 <|> p2 is practically equivalent to the `p1|p2` RegEx)
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) =
    Parser $ \s -> case f s of
      Nothing      -> g s
      Just (a, s') -> Just (a, s')

  -- (p1 <|> p2) <|> p3 = p1 <|> (p2 <|> p3)
  -- p <|> empty = p
  -- empty <|> p = p


-- Basic parsers

-- "end of file" - succeeds on empty input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- Read a character if it satisfies a certain criteria
-- (And the input is not empty.)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- Read a specific character
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- Read any character (. in RegEx)
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- Read a specific string of characters
string :: String -> Parser ()
string str = mapM_ char str

-- Read one or more elements with separators between them
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- Read zero or more elements with separators between them
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []


-- From Control.Applicative:
--
-- Zero or more repetitions (* in RegEx)
-- many :: Parser a -> Parser [a]
--
-- One or more repetitions (+ in RegEx)
-- some :: Parser a -> Parser [a]

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- Zero or one repetition (? in RegEx)
optional :: Parser a -> Parser (Maybe a)
optional p = undefined


-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar = undefined


-- Implement the following RegExes:

-- (ab|cd)+
p1 :: Parser ()
p1 = undefined

-- (foo|bar)*baz
p2 :: Parser ()
p2 = undefined

-- [a-z]
lowercase_ :: Parser ()
lowercase_ = undefined

-- \[foo(, foo)*\]
p3 :: Parser ()
p3 = undefined

-- [a-z]+@domain\.(com|org|hu)
email :: Parser ()
email = undefined
