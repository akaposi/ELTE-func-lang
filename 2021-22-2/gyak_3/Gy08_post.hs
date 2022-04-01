{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy08 where

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
  return a = Parser $ \s -> Just (a, s)   -- nincs hatás

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = mapM_ char s
-- string s = () <$ mapM char s
-- string s = () <$ sequence (map char s)
-- string ""    = return ()
-- string (c:s) = char c >> string s


instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

-- RegEx: *
many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

-- RegEx: +
some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

-- RegEx: ?
optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket!
-- Szükség szerint definiálj segédfüggvényeket.

-- (foo|bar)*kutya?
p0 :: Parser ()
p0 = many_ (string "foo" <|> string "bar") >> string "kuty" >> optional_ (char 'a')

-- \[foo(, foo)*\]$         -- nemüres ,-vel választott foo lista
p1 :: Parser ()
p1 = string "[foo" >> many_ (string ", foo") >> char ']' >> eof

-- (ac|bd)+
p2 :: Parser ()
p2 = some_ (string "ac" <|> string "bd")

-- [a-z], [aeiou]
inList :: [Char] -> Parser Char
inList str = satisfy (`elem` str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

-- std függvény:
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

lowercase :: Parser ()
lowercase = () <$ satisfy isLower

-- [a-z]+@foobar\.(com|org|hu)
p3 :: Parser ()
p3 = undefined

-- -?[0-9]+           -- a -? opcionális '-'-t jelent
p4 :: Parser ()
p4 = undefined

-- ([a-z]|[A-Z])([a-z]|[A-Z]|[0-9])*
p5 :: Parser ()
p5 = undefined

-- ([a-z]+=[0-9]+)(,[a-z]+=[0-9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p6 :: Parser ()
p6 = undefined
