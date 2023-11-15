{-# OPTIONS_GHC -Wno-tabs -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Applicative
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

-- Döntés és Hiba
instance Alternative Parser where
	empty :: Parser a
	empty = Parser (const empty)

	(<|>) :: Parser a -> Parser a -> Parser a
	(<|>) (Parser a) (Parser b) = Parser (\s -> a s <|> b s)

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

-- Szóközök
ws :: Parser ()
ws = void $ many (satisfy isSpace)

-- Top level
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Ebben a fájlban: Token parserek ' karakterrel végződjenek

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

-- Emlékeztető: Functor/Applicative operátorok
--   (<$)       kicserélni parser eredményét adott értékre
--   (<$>)      végrehajt egy függvényt a parser eredményén
--   (<*)       két parser-t futtat, az első értékét visszaadja
--   (*>)       két parser-t futtat, a második értékét visszaadja

-- Most ezeket gyakoroljuk használni!
--------------------------------------------------------------------------------

-- Olvassunk 1 vagy többet, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- Olvassunk 0 vagy többet, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Adott ez a Parser (nemüres "baba"-kat tartalmazó lista)
p0 :: Parser ()
p0 = char '[' <* sepBy1 (string "baba") (char ',') <* char ']'

-- Írjuk át a fenti p0 Parser-t úgy hogy a szavak és szimbólumok
-- között tetszőleges szóközöket elfogadjon. (ws Parser)
p1 :: Parser ()
p1 = undefined

-- Írjuk át a fenti p1 Parser-t úgy hogy a lista hosszát adja vissza
p2 :: Parser Integer
p2 = undefined

-- Adott az integer parser
integer :: Parser Integer
integer = (char '-' $> negate.read <|> pure read) <*> some (satisfy isDigit)

-- És az integer token parser
integer' :: Parser Integer
integer' = integer <* ws

-- Írj egy parsert, ami felismeri Integer-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
integerList :: Parser [Integer]
integerList = undefined

-- pList segédfüggvény: Listát olvasó parser
pList :: Parser a -> Parser [a]
pList = undefined

-- pPair segédfüggvény: Párt olvasó parser
pPair :: Parser a -> Parser b -> Parser (a,b)
pPair = undefined

-- Írj egy parsert, ami felismeri Bool-ok vesszővel elválasztott listáit!
-- Példák: "[]", "[    True , False ]", "[False,False,True,True]"
boolList :: Parser [Bool]
boolList = undefined

-- Írj egy parsert, ami [Maybe Integer] értékeket olvas be Haskell szintaxis szerint!
-- Engedj meg bárhol whitespace-t. Milyen segédfüggvény hiányzik itt?
listMaybeInt :: Parser [Maybe Integer]
listMaybeInt = undefined

-- Írj egy parsert, ami [(Bool, Maybe Integer)] értékeket olvas Haskell
-- szintaxis szerint! Engedj meg bárhol whitespace-t.
listBoolMaybeInt :: Parser [(Bool, Maybe Integer)]
listBoolMaybeInt = undefined

-- Írj egy parsert, ami ([Bool],[Integer]) értékeket olvas Haskell
-- szintaxis szerint! Engedj meg bárhol whitespace-t.
listBoolListInt :: Parser ([Bool],[Integer])
listBoolListInt = undefined

data HTree a = HLeaf a | HNode (HTree a) (HTree a) deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- HTree
treeOf :: Parser a -> Parser (HTree a)
treeOf = undefined

treeOfInt :: Parser (HTree Integer)
treeOfInt = undefined

-- Írj egy Parser-t ami először beolvas egy Integer számot és utána annyi szavat olvas be.
-- Példa: 3 rock is push
-- Példa: 5 flag is stop and sink
lengthPrefixed :: Parser [String]
lengthPrefixed = undefined

-- Bónusz: Írj egy parser-t, ami zárójeleket, + operátorokat és Integer literálokat tartalmazó
-- kifejezéseket olvas! Whitespace-t mindenhol engedj meg.
-- Példa: 10 + 20 + 30
-- Példa: (10 + 20) + 30
-- Példa: 10 + ((20 + 5))

data Exp = Lit Integer | Plus Exp Exp deriving Show

pExp :: Parser Exp
pExp = undefined
