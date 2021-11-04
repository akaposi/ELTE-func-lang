
{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

-- PARSER LIBRARY
--------------------------------------------------------------------------------

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a


-- Parser a : String-ből "a" típusú értéket próbál olvasni
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where

  -- nem dob hibát + nem fogyaszt inputot
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- egymás után két parsert hívunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- parserek közötti választás
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- üres String olvasása
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- Char olvasása, amire egy feltétel teljesül
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

-- parser hívásakor kiír egy String üzenetet
debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- konkrét Char olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- bármilyen Char olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét String-et próbál olvasni
string :: String -> Parser ()
string = traverse_ char

-- Control.Applicative-ból:
-- many :: Parser a -> Parser [a]        -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]        -- 1-szer vagy többször olvasás

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

-- whitespace elfogyasztása
ws :: Parser ()
ws = () <$ many (char ' ')

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit


-- FELADATOK
--------------------------------------------------------------------------------

-- implementáld a következő regex-eket. Szükség szerint definiálj segédfüggvényeket.

-- (foo|bar)*kutya
p01 :: Parser ()
p01 = undefined

-- \[foo(, foo)*\]         -- nemüres ,-vel választott foo lista
p02 :: Parser ()
p02 = undefined

-- (ac|bd)*
p1 :: Parser ()
p1 = undefined

-- [a..z]+@foobar\.(com|org|hu)
p2 :: Parser ()
p2 = undefined

-- -?[0..9]+           -- a -? opcionális '-'-t jelent
p3 :: Parser ()
p3 = undefined

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p4 :: Parser ()
p4 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p5 :: Parser ()
p5 = undefined

--------------------------------------------------------------------------------

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = undefined

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined





-- Írj egy parser-t ami, zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas!
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




-- bónusz (nehéz): írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
