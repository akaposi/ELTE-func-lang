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
-- p0 = string "0x" >> some (inList (['0'..'9'] ++ ['A'..'F'])) >> eof
p0 = do
  string "0x"
  some (inList (['0'..'9'] ++ ['A'..'F']))
  eof

posHex :: Parser Int
posHex = do
  string "0x"
  digits <- some (inList (['0'..'9'] ++ ['A'..'F']))
  eof
  let values = map digitToInt digits
  return (foldl (\acc curr -> acc * 16 + curr) 0 values)

hexCharsToInt :: String -> Int
hexCharsToInt hc = foldl (\acc curr -> acc * 16 + curr) 0 (map digitToInt hc)

posHex' :: Parser Int
posHex' = hexCharsToInt <$> (string "0x" *> some (inList (['0'..'9'] ++ ['A'..'F'])) <* eof)

p1 :: Parser ()
p1 = do
  string' "["
  string' "foo"
  many_ (string' "," >> string' "foo")
  char ']'
  eof

p2 :: Parser ()
p2 = do
  string' "["
  sepBy (string' "foo") (char' ',')
  char ']'
  eof

listLength :: Parser Int
listLength = do
  string' "["
  foos <- sepBy (string' "foo") (char' ',')
  char ']'
  eof
  return (length foos)

listLength' :: Parser Int
listLength' = length <$> (string' "[" *> (sepBy (string' "foo") (char' ',')) <* char ']')

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> inList ['0'..'9']

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = do
  digits <- some digit
  return (foldl (\acc curr -> acc * 10 + curr) 0 digits)

posInt' :: Parser Int
posInt' = posInt <* ws

pList :: Parser a -> Parser [a]
pList pa = char' '[' *> sepBy pa (char' ',') <* char' ']'

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = pList posInt'

pBool :: Parser Bool
pBool = True <$ string' "True" <|> False <$ string' "False"

-- Írj egy parsert, ami felsimeri Bool-ok vesszővel elválasztott listáit!
-- Példák: "[]", "[    True , False ]", "[False,False,True,True]"
boolList :: Parser [Bool]
boolList = pList pBool
-- boolList = do
--   string' "["
--   bools <- sepBy (True <$ string' "True" <|> False <$ string' "False") (char' ',')
--   char ']'
--   return bools

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
  [ "Isti is a 25 years old boy."
  , "Mary is a 29 years old girl."
  , "Liza is an 8 years old girl."
  , "Peter is a 1 year old boy."
  , "Mary is a 29 years old girl"
  , "liza is an 8 years old girl."
  , "Peter is a twelve year old boy."
  ]

-- Csak egyezés vizsgálata
person' :: Parser ()
person' = do
  inList ['A'..'Z']
  some (inList ['a'..'z'])
  string " is a " <|> string " is an "
  posInt
  string " year old " <|> string " years old "
  string "girl" <|> string "boy"
  char '.'

-- Információ kinyerése
person :: Parser Person
person = do
  name1 <- inList ['A'..'Z']
  name2 <- some (inList ['a'..'z'])
  string " is a " <|> string " is an "
  age <- posInt
  string " year old " <|> string " years old "
  gender <- Girl <$ string "girl" <|> Boy <$ string "boy"
  char '.'
  return (MkPerson (name1:name2) age gender)
