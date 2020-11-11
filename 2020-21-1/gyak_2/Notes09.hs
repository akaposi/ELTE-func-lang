{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MonadComprehensions #-}
module Notes09 where

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad 

-------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Just (x, s') -> Just (f x, s')
    Nothing      -> Nothing

instance Applicative Parser where pure = return; (<*>) = ap

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  Parser p >>= f = Parser $ \s -> case p s of
    Just (x, s') -> runParser (f x) s'
    Nothing      -> Nothing

-------------------------------------------------------------------------------

-- The `eof` (end of file) parser.
--  succeeds if the input string is empty, fails otherwise.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- The parser `satisfy p` succeeds if the input string starts with a character 
--  that satisfies the predicate p (and consumes that character), and fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

-------------------------------------------------------------------------------

-- The parser `char c` should succeed if the input string start with the character c.
-- Examples:
--   runParser (char 'a') "" == Nothing
--   runParser (char 'a') "abc" == Just ((), "bc")
--   runParser (char 'a') "bcd" == Nothing
char :: Char -> Parser ()
char c = satisfy (== c) *> pure ()

-- The parser anyChar should succeed if the input string is not empty, and return its first character.
-- Examples:
--   runParser anyChar "" == Nothing
--   runParser anyChar "()" == Just ('(', ")")
--   runParser anyChar "abc" == Just ('a', "bc")
anyChar :: Parser Char
anyChar = satisfy (const True)

-- The parser `string s` should succeed if the input string starts with the string s.
--   runParser (string "abc") "abdef" == Nothing
--   runParser (string "") "abcdef" == Just ((), "abcdef")
--   runParser (string "abc") "abcdef" == Just ((), "def")
string :: String -> Parser ()
string s = forM_ s char

space :: Parser ()
space = satisfy isSpace *> pure ()

ws :: Parser ()
ws = many space *> pure ()

-------------------------------------------------------------------------------

-- class Applicative f => Alternative f where
--   empty :: f a 
--   (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  -- The parser `empty` always fails.
  empty = Parser $ \_ -> Nothing

  -- The parser `p1 <|> p2` tries the parser p1. 
  --  If p1 succeeds, it returns the result of p1.
  --  If p1 fails, then it tries the parser p2 instead.
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-------------------------------------------------------------------------------

-- Use `some` and/or `many` and/or `<|>` to define sepBy1 and sepBy.

-- `sepBy1 p sep` parses the regular expression p (sep p)*, 
--   i.e. any sequence of the form
--     p 
--     p sep p
--     p sep p sep p
--     ...
-- It fails if it cannot parse p at least once.  
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepByWithSep1 :: Parser a -> Parser sep -> Parser (a, [(sep, a)])
sepByWithSep1 p sep = (,) <$> p <*> many ((,) <$> sep <*> p)

-- sepBy p sep parses either any sequence of the form
--     p 
--     p sep p
--     ...
--   or returns the empty list.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-------------------------------------------------------------------------------

digit :: Parser Integer
digit = fromIntegral . digitToInt <$> satisfy isDigit

posInt :: Parser Integer
posInt = foldl' (\x y -> 10*x+y) 0 <$> some digit

int :: Parser Integer
int = negInt <|> posInt
  where negInt = char '-' *> (negate <$> posInt)

data Tree a = Leaf a 
            | Node [Tree a]
            deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- pIntTree should parse trees of integers.
-- Examples:
--   runParser pIntTree "0"                ~~>  Leaf 0
--   runParser pIntTree "[]"               ~~>  Node []
--   runParser pIntTree "[0, 1]"           ~~>  Node [Leaf 0, Leaf 1]
--   runParser pIntTree "[[], [2]]"        ~~>  Node [Node [], Node [Leaf 2]]
--   runParser pIntTree "[0, [1], [[2]]]"  ~~>  Node [Leaf 0, Node [Leaf 1], Node [Node [Leaf 2]]]

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

char' c = lexeme (char c)
int' = lexeme int

brackets :: Parser a -> Parser a
brackets p = char' '[' *> p <* char' ']'

pIntTree :: Parser (Tree Integer)
pIntTree = (Leaf <$> int')
       <|> (Node <$> brackets (sepBy pIntTree (char' ',')))

-------------------------------------------------------------------------------

data IntExpr = Value Integer
             | Plus  IntExpr IntExpr
             | Minus IntExpr IntExpr
             | Times IntExpr IntExpr
             | Div   IntExpr IntExpr
             deriving (Eq, Ord, Show)

-- Parser for expressions built from 
--   integer constants,
--   +, -, *, /
--   parentheses


parens :: Parser a -> Parser a
parens p = char' '(' *> p <* char' ')'



-- We define several subparsers, to deal with the diffferent precedences of the operators.

-- pValueOrParens should either parse an integer value, 
--   or any expression wrapped in parentheses.
pValueOrParens :: Parser IntExpr
-- pExprTimes should parse any expression built from pValueOrParens, * and /
pExprTimes     :: Parser IntExpr
-- pExprPlus should parse any expression built from pExprTimes, + and -
pExprPlus      :: Parser IntExpr

pExpr          :: Parser IntExpr

pValueOrParens = (Value <$> int')
             <|> parens pExpr

-- pExprTimes = _ <$> sepByWithSep1 pValueOrParens (char '*' <|> char '/')
pExprTimes = do acc <- pValueOrParens; go acc
  where go acc = goTimes acc
             <|> goDiv acc
             <|> pure acc
        goTimes x = do char' '*'; y <- pValueOrParens; go (Times x y)
        goDiv x   = do char' '/'; y <- pValueOrParens; go (Div x y)

pExprPlus = do acc <- pExprTimes; go acc
  where go acc = goPlus acc
             <|> goMinus acc
             <|> pure acc
        goPlus x = do char' '+'; y <- pExprTimes; go (Plus x y)
        goMinus x   = do char' '-'; y <- pExprTimes; go (Minus x y)

pExpr          = pExprPlus

-- Examples:
--  runParser pExpr  "((((2))))"       == Value 2
--  runParser pExpr  "1 * 2 / 3 * 4"   == ((Value 1 `Times` Value 2) `Div` Value 3) `Times` Value 4
--  runParser pExpr  "1 + 2 - 3 + 4"   == ((Value 1 `Plus` Value 2) `Minus` Value 3) `Plus` Value 4
--  runParser pExpr  "1 * 2 + 3 * 4"   == (Value 1 `Times` Value 2) `Plus` (Value 3 `Times` Value 4)
--  runParser pExpr  "1 * (2 + 3) * 4" == Value 1 `Times` (Value 2 `Plus` Value 3) `Times` Value 4