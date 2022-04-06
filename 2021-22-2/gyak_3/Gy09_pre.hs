{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}
module Gy09 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string s = mapM_ char s

instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
-- ∙ many  :: Parser a -> Parser [a]
-- ∙ some  :: Parser a -> Parser [a]

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- ∙ optional :: Parser a -> Parser (Maybe a)

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

inList :: [Char] -> Parser Char
inList str = satisfy (`elem` str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

------------------------------------------------------------

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- token/whitespace parsing segédfüggvények

ws :: Parser ()
ws = many_ (satisfy isSpace)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- operátor segédfüggvények

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

-- Functor/Applicative operátorok
--   (<$)       kicserélni parser végeredményét adott értékre
--   (<$>)      végrehajt egy függvényt a parser eredményén (fmap)
--   (<*)       két parser-t futtat, az első értékét visszaadja
--   (*>)       két parser-t futtat, a második értékét visszaadja

--------------------------------------------------------------------------------

{- Example -}

-- Parser, that matched hexadecimal number literals of the followig format:
--
-- 0x[0-9A-F]+$
--
-- Valid examples:
-- + "0x0"
-- + "0xFA55B8"
--
-- Invalid examples:
-- + "1337"
-- + "0x1Q34"

p0 :: Parser ()
p0 = undefined

posHex :: Parser Int
posHex = undefined

-- Upgrade this to accept whitespace! (use `sepBy`)
-- \[foo(, foo)*\]$ -- nemüres ,-vel választott foo lista
p1 :: Parser ()
p1 = string "[foo" >> many_ (string ", foo") >> char ']' >> eof

listLength :: Parser Int
listLength = undefined

-- egy számjegy olvasása
digit :: Parser Int
digit = undefined

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = undefined

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined

-- Írj egy parsert, ami felsimeri Bool-ok vesszővel elválasztott listáit!
-- Példák: "[]", "[    True , False ]", "[False,False,True,True]"
boolList :: Parser [Bool]
boolList = undefined

-- Írj egy parsert, ami [Maybe Int] értékeket olvas be Haskell szintaxis szerint!
-- Engedj meg bárhol whitespace-t.
listMaybeInt :: Parser [Maybe Int]
listMaybeInt = undefined

-- Extra: Írj egy parsert, ami [(Bool, Maybe Int)] értékeket olvas Haskell
-- szintaxis szerint! Engedj meg bárhol whitespace-t.
-- Javasolt segédfüggvény: `pPair`
listBoolMaybeInt :: Parser [(Bool, Maybe Int)]
listBoolMaybeInt = undefined

-- Extra: Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat
-- ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- Parse a description of a person in the following format:
-- ∙ Begins with a name, which is a single uppercase letter followed by one or
--   more lowercase letters
-- ∙ Followed by " is a " or " is an " [hint: the 'n' is optional]
-- ∙ Followed by a number (one or more digits)
-- ∙ Followed by " year old " or " years old " [hint: the 's' is optional]
-- ∙ Followed by either "girl" or "boy"
-- ∙ Ends with a period

data Gender = Boy | Girl deriving (Eq, Show)
data Person = MkPerson { name :: String, age :: Int, gender :: Gender }
              deriving (Eq, Show)

people =
  [ "Isti is a 24 years old boy."
  , "Mary is a 29 years old girl."
  , "Liza is an 8 years old girl."
  , "Peter is a 1 year old boy."
  ]

person :: Parser Person
person = undefined

-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! (Lásd előadás) Whitespace-t mindenhol engedj meg.
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp = Lit Int | Plus Exp Exp deriving Show

pExp :: Parser Exp
pExp = undefined


-- Extra: írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
