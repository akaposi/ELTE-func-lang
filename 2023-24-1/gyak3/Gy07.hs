{-# OPTIONS_GHC -Wno-tabs -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative hiding (some,many,optional,choice)
import Control.Monad
import Data.Functor
import Data.Char

-- Parser
--------------------------------------------------------------------------------

-- Válassz nehézségi szintet:

-- Könnyű mód --
-- newtype Parser a = Parser {runParser :: String -> [(a, String)]}

-- Nagyon könnyű mód --
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

--------------------------------------------------------------------------------

instance Functor Parser where
	fmap :: (a -> b) -> Parser a -> Parser b
	fmap = liftM

instance Applicative Parser where
	pure :: a -> Parser a
	pure = Parser . curry pure

	(<*>) :: Parser (a -> b) -> Parser a -> Parser b
	(<*>) = ap

instance Monad Parser where
	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	(>>=) (Parser p1) f = Parser (p1 >=> uncurry (runParser . f))

-- Kizárólag az üres bemenetet ismeri fel
eof :: Parser ()
eof = Parser $ \s -> case s of
	[] -> pure ((), [])
	_  -> empty

-- Egyetlen, az adott feltételt teljesítő karaktert fogad el
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
	[] -> empty
	(x:xs) -> (x, xs) <$ guard (f x)

-- A konkrét karaktert fogadja el
char :: Char -> Parser ()
char c = void $ satisfy (==c)

-- A konkrét szöveget fogadja el
string :: String -> Parser ()
string [] = pure ()
string (x:xs) = char x *> string xs

-- Döntés és Hiba
instance Alternative Parser where
	empty :: Parser a
	empty = Parser (const empty)

	(<|>) :: Parser a -> Parser a -> Parser a
	(<|>) (Parser a) (Parser b) = Parser (\s -> a s <|> b s)

-- optional: Futtatja v-t, ha nem sikerül Nothing-ot ad vissza
optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing

-- many: 0-szor vagy többször futtatja
many :: Alternative f => f a -> f [a]
many v = some v <|> pure []

-- some: 1-szer vagy többször futtatja
some :: Alternative f => f a -> f [a]
some v = (:) <$> v <*> many v

-- Hiba szöveggel. Do notációban hasznos!
instance MonadFail Parser where
	fail :: String -> Parser a
	fail = const empty

-- Alternative és Monad kombinációja
instance MonadPlus Parser where
	mzero :: Parser a
	mzero = empty

	mplus :: Parser a -> Parser a -> Parser a
	mplus = (<|>)

-- Parser feladatok
--------------------------------------------------------------------------------

--   Regex     Parser
----------------------
--   c         char c
--   p|q       p <|> q
--   pq        p *> q
--   p*        many p
--   p+        some p
--   ε         eof

-- Implementáld a következő regex-eket!
-- Szükség szerint definiálj segédfüggvényeket.

-- (ba|ke)+
p0 :: Parser ()
p0 = undefined

-- crab(,crab)* -- nemüres vesszővel választott crab lista
-- Példa: "crab", "crab,crab"
p1 :: Parser ()
p1 = undefined

-- (ba|ke)*rock
p2 :: Parser ()
p2 = undefined

-- [a-z]+@hedge\.(com|org|hu)
p3 :: Parser ()
p3 = undefined

-- \-?[0..9]+    -- Szám opcionális - előjellel
p4 :: Parser ()
p4 = undefined

-- [a-zA-Z][a-zA-Z0-9]* -- Számjegyekből és betűkből álló sorozat ami betűvel kezdődik
p5 :: Parser ()
p5 = undefined

-- ([a-z]+=[0-9]+)(,[a-z]+=[0-9]+)*
-- Példa:   "baba=10,flag=30,skull=40"
p6 :: Parser ()
p6 = undefined

-- Bónusz feladat
-- [a-z]*win -- A megoldás itt függ a nehézségi szinttől!
p7 :: Parser ()
p7 = undefined

-- További Parser feladatok
--------------------------------------------------------------------------------

-- Bármelyik Char a listából
inList :: [Char] -> Parser Char
inList str = undefined

-- Bármelyik Parser a listából
choice :: [Parser a] -> Parser a
choice = undefined

-- Egy kisbetűt olvassunk be
lowercase :: Parser Char
lowercase = undefined

-- Egy számjegyet olvassunk be
digit :: Parser Int
digit = undefined

-- Egy integer értéket olvassunk be
-- Opcionálisan megelőzheti egy - előjel
integer :: Parser Integer
integer = undefined

-- Írj egy parsert, ami felismeri Integer-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[ 12  , 34555 ]", "[0,1,2,3]"
integerList :: Parser [Integer]
integerList = undefined

-- Írj egy parsert ami whitespace szövegeket ismer fel.
-- Példák: "", " ", "    "
-- Használd az isSpace függvényt!
ws :: Parser ()
ws = undefined

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balanced :: Parser ()
balanced = undefined

-- Beolvasunk egy egész számot amit whitespace előzhet meg
readInteger :: Parser Integer
readInteger = ws *> integer

-- Ugyanaz mint a mai +/-, csak Parser monáddal
breakAt :: Integer -> Parser [Integer]
breakAt = undefined
