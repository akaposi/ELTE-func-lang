{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

module Gy08 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace

-- PARSER LIBRARY
--------------------------------------------------------------------------------

-- newtype State s a = State {runState :: s -> (a, s)}

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
string "" = pure ()
string (c : s) = do  -- char c >> string s
  char c
  string s
-- string = mapM_ char


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
-- Szükség szerint definiálj segédfüggvényeket.

-- (foo|bar)*(kutya)?
-- foo|bar
p0 :: Parser ()
p0 = many_ (string "foo" <|> string "bar") >> optional_ (string "kutya")

-- \[foo(, foo)*\]         -- nemüres ,-vel választott foo lista
p1 :: Parser ()
p1 = string "[foo" >> many_ (string ", foo") >> char ']'

-- (ac|bd)+
p2 :: Parser ()
p2 = some_ (string "ac" <|> string "bd")

-- Olyan karaktert olvasunk be, amit tartalmaz a lista
inList :: [Char] -> Parser Char
inList str = satisfy (\c -> elem c str)

inList_ :: [Char] -> Parser ()
inList_ str = () <$ inList str

-- std függvény:
choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) empty ps

-- amíg tud lowercase karaktereket olvasni olvasson.
lowercase :: Parser ()
-- lowercase = many_ (inList ['a'..'z'])
lowercase = many_ (satisfy isLower)

-- [a..z]+@foobar\.(com|org|hu)
p3 :: Parser ()
p3 = satisfy isLower >> lowercase >> string "@foobar." >> choice [string "com", string "org", string "hu"]

-- -?[0..9]+           -- a -? opcionális '-'-t jelent
p4 :: Parser ()
p4 = undefined

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p5 :: Parser ()
p5 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p6 :: Parser ()
p6 = undefined

------------------------------------------------------------

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

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

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined