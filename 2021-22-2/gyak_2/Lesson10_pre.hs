module Lesson10 where

import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    -- g :: String -> Maybe (a, String)
    fmap f (Parser g) = Parser $ \str -> case g str of
        Nothing        -> Nothing
        Just (a, str') -> Just (f a, str')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \str -> Just (a, str)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -- f :: String -> Maybe (a -> b, String)
    -- g :: String -> Maybe (a, String)
    (Parser f) <*> (Parser g) = Parser $ \str -> case f str of
        Nothing           -> Nothing
        Just (aToB, str') -> case g str' of
            Nothing         -> Nothing
            Just (a, str'') -> Just (aToB a, str'')

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser f) >>= g = Parser $ \str -> case f str of
        Nothing       -> Nothing
        Just (a,str') -> runParser (g a) str'
    
-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \str -> case str of
    [] -> Just ((),[])
    _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \str -> case str of
    (x:xs) | p x -> Just (x,xs)
    _            -> Nothing

-- olvassunk egy tetszőleges karaktert
anyChar :: Parser Char
anyChar = satisfy (const True)

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = void (satisfy (== c))

-- char c = () <$ satisfy (== c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string []     = pure ()
string (x:xs) = do
    char x
    string xs

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing 
    
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser f) <|> (Parser g) = Parser $ \str -> case f str of
        Nothing -> g str
        x       -> x

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

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)
--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket!
-- Szükség szerint definiálj segédfüggvényeket.

-- (foo|bar)*kutya?
p0 :: Parser ()
p0 = do
    many (string "foo" <|> string "bar")
    string "kuty"
    optional_ (char 'a')
    eof

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

data Exp = Lit Integer | Plus Exp Exp | Mul Exp Exp

evalExp :: Exp -> Integer
evalExp = undefined

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = undefined

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = undefined

ws :: Parser ()
ws = undefined

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

pExp :: Parser Exp
pExp = undefined