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

-- Természetes szám
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

-- Egész szám
integer :: Parser Integer
integer = (negate <$ char '-' <|> pure id) <*> natural

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

natural' :: Parser Integer
natural' = natural <* ws

integer' :: Parser Integer
integer' = integer <* ws

-- Emlékeztető: Functor/Applicative operátorok
--   (<$)       kicserélni parser eredményét adott értékre
--   (<$>)      végrehajt egy függvényt a parser eredményén
--   (<*)       két parser-t futtat, az első értékét visszaadja
--   (*>)       két parser-t futtat, a második értékét visszaadja

-- Szintaxis segédfüggvények
--------------------------------------------------------------------------------

-- Szóköz
ws :: Parser ()
ws = void $ many (satisfy isSpace)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- Top level
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Operátor segédfüggvények
rightAssoc :: (a -> a -> a) -> Parser a -> Parser (a -> a -> a) -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = pa >>= \left -> f left <$> (psep *> pa) <|> pure left

prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa

-- Szintaxisfa
--------------------------------------------------------------------------------

data Exp =
	  NatLit Integer    -- egész szám
	| Plus Exp Exp      -- e + e
	| Mul Exp Exp       -- e * e
	| Var String        -- szimbólum
	| BoolLit Bool      -- bool érték
	| Not Exp           -- not e
	| Eq Exp Exp        -- e == e
	deriving Show

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - ! alkalmazás
--   - *     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

-- pExp :: Parser Exp
-- pExp-et használjuk rekurzívan néhány Parser-ben de a végén fogjuk definiálni.

-- Szimbólum (változónév)
pVar :: Parser Exp
pVar = undefined

-- Természetes szám literál
pNat :: Parser Exp
pNat = undefined

-- Zárójelekkel körbezárt kifejezés
pEnclosed :: Parser Exp
pEnclosed = undefined

-- Környezetfüggetlen kifejezés
atom :: Parser Exp
atom = undefined

pNot :: Parser Exp
pNot = undefined

pMul :: Parser Exp
pMul = undefined

pAdd :: Parser Exp
pAdd = undefined

pEq :: Parser Exp
pEq = undefined

pExp :: Parser Exp
pExp = undefined

-- Írjuk meg a kifejezés kiértékelését!
-- A paraméterül kapott függvény segítségével helyettesítsük be a Var értékeket.
substExp :: (String -> Exp) -> Exp -> Exp
substExp = undefined

-- További gyakorlásként adjunk hozzá a kifejezésfához még több operátort.

-- '-' prefix és infix operátor
-- Bónusz: Mire van szükségünk hogy azonos kötése legyen mint a + operátornak?

-- '&&' és '||' logikai operátorok (&& köt erősebben)


-- Bónusz: Írjunk parser-t típusozatlan lambda kalkuluszhoz!
--------------------------------------------------------------------------------

-- példák : \f x -> f x
--          (\x -> x) (\x -> x)
--          (\f x y -> f) x (g x)

data Tm = TVar String | App Tm Tm | Lam String Tm deriving Show

subst :: String -> Tm -> Tm -> Tm
subst s k (TVar z) = if s == z then k else (TVar z)
subst s k (App a b) = App (subst s k a) (subst s k b)
subst s k (Lam z a) = Lam z (if s == z then a else (subst s k a))

reduce :: Tm -> Tm
reduce (App a b) = case reduce a of
	(Lam s k) -> reduce $ subst s b k
	a -> App a b
reduce s = s

pTm :: Parser Tm
pTm = undefined