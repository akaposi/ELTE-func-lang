{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy09 where

import Control.Monad
import Control.Applicative
import Data.Char  -- isDigit, isAlpha, digitToInt

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

inList :: [Char] -> Parser Char
inList str = satisfy (`elem` str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str


--   regex     Parser
----------------------
--     c       char c
--    p|q     p <|> q
--    pq      p >> q
--    p*      many_ p
--    p+      some_ p
--    ε       eof
--    ?       optional_ p

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)


-- Whitespace kezelése
------------------------------------------------------------

ws :: Parser ()
ws = many_ (satisfy (\c -> c == ' ' || c == '\n'))  -- vagy: Data.Char.isSpace

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Ebben a fájlban: token parserek '-s nevűek legyenek

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws


-- Functor/Applicative operátorok
--   (<$)       kicserélni parser végeredményét adott értékre
--   (<$>)      végrehajt egy függvényt a parser eredményén (fmap)
--   (<*)       két parser-t futtat, az első értékét visszaadja
--   (*>)       két parser-t futtat, a második értékét visszaadja
--------------------------------------------------------------------------------

{- Example -}

-- Parser, that matched hexadecimal number literals of the followig format:
--
-- 0x[0-9A-F]+ε
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

-- pList segédfüggvény: Lista olvasó parser
pList :: Parser a -> Parser [a]
pList = undefined

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
  [ "Dominik is a 20 years old boy."
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