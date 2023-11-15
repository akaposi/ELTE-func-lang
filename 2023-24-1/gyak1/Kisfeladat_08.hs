{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Kiszh8 where

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
-- <$>

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string = mapM_ char 
-- string "" = pure ()
-- string (c:str) = char c >> string str


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

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

inList :: [Char] -> Parser Char
inList str = satisfy (\c -> elem c str)
-- `elem` str

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

--------------------------------------------------------------------------------

--   regex     Parser
----------------------
--     c       char c
--    p|q     p <|> q
--    pq      p >> q
--    p*      many_ p
--    p+      some_ p
--    ε       eof
--    ?       optional_ p

-- Implementáld a következő regex-eket!
-- Ha kell, írj segédfüggvényt.

-- (alma|kompot(ok)?)*ε
p0 :: Parser ()
p0 = undefined

testsP0 :: [Bool]
testsP0 = 
    [
        runParser p0 "alma" == Just ((), ""),
        runParser p0 "kompot" == Just ((), ""),
        runParser p0 "kompotok" == Just ((), ""),
        runParser p0 "almakompot" == Just ((), ""),
        runParser p0 "kompotalma" == Just ((), ""),
        runParser p0 "kompotokalma" == Just ((), ""),
        runParser p0 "almakompotok" == Just ((), ""),
        runParser p0 "almakompotokalmaalma" == Just ((), ""),
        runParser p0 "" == Just ((), ""),
        runParser p0 "alma " == Nothing,
        runParser p0 "valami" == Nothing
    ]

-- [0-9]+\.(kis)?ZH
p1 :: Parser ()
p1 = undefined

testsP1 :: [Bool]
testsP1 = 
  [
      runParser p1 "8.kisZH" == Just ((), ""),
      runParser p1 "8.kisZH almafa" == Just ((), " almafa"),
      runParser p1 "1.ZH" == Just ((), ""),
      runParser p1 "12231231.kisZH" == Just ((), ""),
      runParser p1 "12.kis" == Nothing,
      runParser p1 ".kisZH" == Nothing,
      runParser p1 "" == Nothing,
      runParser p1 "000kisZh" == Nothing,
      runParser p1 "valami" == Nothing
  ]