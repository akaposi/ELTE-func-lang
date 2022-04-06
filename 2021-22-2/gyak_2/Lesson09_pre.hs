module Lesson09 where

import Control.Monad
import Control.Applicative
import Data.Char

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a

-- instance Functor Parser where
-- instance Applicative Parser where

-- instance Monad Parser where

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = undefined

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = undefined

-- olvassunk egy tetszőleges karaktert
anyChar :: Parser Char
anyChar = undefined

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = undefined

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = undefined

-- instance Alternative Parser where

-- Control.Applicative-ból:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

many' :: Parser a -> Parser [a]
many' = undefined

some' :: Parser a -> Parser [a]
some' = undefined

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- általában ezt úgy szokás megtalálni, hogy `between :: Parser open -> Parser close -> Parser a -> Parser a
between :: Parser open -> Parser a -> Parser close -> Parser a
between = undefined
-- Ezt általánosabban is meg lehet írni.

--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket!
-- Szükség szerint definiálj segédfüggvényeket.

-- (foo|bar)*kutya?
p0 :: Parser ()
p0 = undefined

-- \[foo(, foo)*\]         -- nemüres ,-vel választott foo lista
p1 :: Parser ()
p1 = undefined

-- (ac|bd)+
p2 :: Parser ()
p2 = undefined

inList :: [Char] -> Parser Char
inList str = undefined

inList_ :: [Char] -> Parser ()
inList_ str = undefined

-- std függvény:
choice :: [Parser a] -> Parser a
choice ps = undefined

lowercase :: Parser ()
lowercase = undefined

-- [a..z]+@foobar\.(com|org|hu)
p3 :: Parser ()
p3 = undefined

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
   --   (<*)       két parser-t futtat balról jobbra, az első értékét visszaadja
   --   (*>)       két parser-t futtat balról jobbra, a második értékét visszaadja


-- Olvass be egy Integer-t! (Számjegyek nemüres sorozata)
integer :: Parser Integer
integer = undefined

-- Írj egy parsert, ami felsimeri Integer-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Integer]
intList = undefined

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined
