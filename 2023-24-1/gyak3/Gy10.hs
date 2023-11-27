{-# OPTIONS_GHC -Wno-tabs -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Foldable
import Data.Char
import Prelude hiding (lookup)

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

-- Csak azokat a bemeneteket ismeri fel amiket az adott minta nem ismer fel.
negative :: Parser a -> Parser ()
negative (Parser p) = Parser $ \s -> if null (p s) then pure ((), s) else empty

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

-- Szintaxis segédfüggvények
--------------------------------------------------------------------------------

-- Szóköz
ws :: Parser ()
ws = void $ many (satisfy isSpace)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

relaxedSepBy :: Parser a -> Parser sep -> Parser [a]
relaxedSepBy pa psep = concatMap toList <$> sepBy (optional pa) psep

-- Top level
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Operátor segédfüggvények
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
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
	  IntLit Integer      -- int literál (pozitív)
	| Add Exp Exp         -- e + e
	| Sub Exp Exp         -- e - e
	| Mul Exp Exp         -- e * e
	| BoolLit Bool        -- true|false
	| And Exp Exp         -- e && e
	| Or Exp Exp          -- e || e
	| Not Exp             -- not e
	| Eq Exp Exp          -- e == e
	| Var String          -- (változónév)
	deriving (Eq, Show)

{-
Változónév: nemüres alfabetikus string, ami nem kulcs szó
Kötési erősségek csökkenő sorrendben:
  - atom: zárójelezett kifejezés, literál, változónév
  - not alkalmazás
  - *  : jobbra asszoc
  - +  : jobbra asszoc
  - -  : jobbra asszoc
  - && : jobbra asszoc
  - || : jobbra asszoc
  - == : nem asszoc
-}

keywords :: [String]
keywords = ["not", "true", "false", "while", "if", "do", "end", "then", "else", "print"]

identifier :: Parser String
identifier = do
	x <- some (satisfy isAlpha)
	guard (not (elem x keywords))
	ws
	return x

keyword :: String -> Parser ()
keyword s = do
	string s
	negative (satisfy isAlpha)
	ws

-- Szimbólum (változónév)
pVar :: Parser Exp
pVar = Var <$> identifier

-- Szám literál
pNat :: Parser Exp
pNat = IntLit <$> integer'

-- Bool literál
pBool :: Parser Exp
pBool = BoolLit <$> (string' "true" $> True <|> string' "false" $> False)

-- Zárójelekkel körbezárt kifejezés
pEnclosed :: Parser Exp
pEnclosed = char' '(' *> pExp <* char' ')'

-- Környezetfüggetlen kifejezés
atom :: Parser Exp
atom = pVar <|> pNat <|> pBool <|> pEnclosed

pNot :: Parser Exp
pNot = prefix Not atom (string' "not")

pMul :: Parser Exp
pMul = rightAssoc Mul pNot (char' '*')

pSub :: Parser Exp
pSub = rightAssoc Sub pMul (char' '-')

pAdd :: Parser Exp
pAdd = rightAssoc Add pSub (char' '+')

pAnd :: Parser Exp
pAnd = rightAssoc And pAdd (string' "&&")

pOr :: Parser Exp
pOr = rightAssoc And pAnd (string' "||")

pEq :: Parser Exp
pEq = nonAssoc Eq pOr (string' "==")

pExp :: Parser Exp
pExp = pEq

-- While nyelv parsolása
--------------------------------------------------------------------------------

type Program = [Statement]  -- st1; st2; st3; ... st4

data Statement
	= Assign String Exp       -- <var> := <e>
	| While Exp Program       -- while <e> do <prog> end
	| If Exp Program Program  -- if <e> then <prog> else <prog> end
	| Print Exp
	deriving (Eq, Show)

-- Statement szintaxisban nem kell precendenciával foglalkozni, mert valójában
-- csak "atomi" konstrukció van
statement :: Parser Statement
statement = undefined

-- Az utasításokat ';'-vel választjuk el.
-- A probléma hogy néha az utolsó utasítást is egy ';' követ.
-- Ezért hozzáadtam egy relaxedSepBy segédfüggvényt.
program :: Parser Program
program = undefined

-- StateF
--------------------------------------------------------------------------------

-- Emlékeztető: State monáddal az 5. és 6. gyakorlatban foglalkoztunk.
-- Ez egy kiterjesztett State monád ami támogat egy külön hibaállapotot.
-- instance Monad (Either a) definiálva van.

newtype StateF s a = StateF {runState :: s -> Either String (a , s)}

instance Functor (StateF s) where
	fmap = liftM

instance Applicative (StateF s) where
	(<*>) = ap
	pure = return

instance Monad (StateF s) where
	return a = StateF $ \s -> Right (a , s)
	(>>=) m k = StateF $ \s0 -> runState m s0 >>= \(a, s1) -> runState (k a) s1

instance MonadFail (StateF s) where
	fail = StateF . const . Left

evalStateFail :: StateF s a -> s -> Either String a
evalStateFail = (fmap fst .) . runState

execStateFail :: StateF s a -> s -> Either String s
execStateFail = (fmap snd .) . runState

get :: StateF s s
get = StateF (\s -> Right (s, s))

put :: s -> StateF s ()
put = StateF . const . Right . (,) ()

modify :: (s -> s) -> StateF s ()
modify f = get >>= put . f

-- While nyelv értelmezése
--------------------------------------------------------------------------------

data Val = VInt Integer | VBool Bool
  deriving (Eq, Show)

-- Környezet: A definiált változókat és értékeiket tartalmazza
type Env = [(String, Val)]

lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup a [] = Nothing
lookup a ((x,y):s) = if a == x then Just y else lookup a s

-- Ha az adott nevű változó létezik, írjuk felül az értékét az Env-ben.
-- Ha a változó nem létezik hozzuk létre a lista végén.
updateEnv :: String -> Val -> Env -> Env
updateEnv = undefined

-- Értékeljük ki a kifejezést. Ha nem sikerül, adjunk vissza egy String üzenetet a hibáról.
evalExp :: Exp -> Env -> Either String Val
evalExp = undefined

-- Értékeljük ki a bemenet StateF-t de utána dobjuk el az általa definiált új változókat.
-- Milyen lista függvények segítenek itt?
inNewScope :: StateF Env a -> StateF Env a
inNewScope = undefined

-- Értékeljük ki az utasítást.
evalStatement :: Statement -> StateF Env String
evalStatement = undefined

-- Futtassuk a programot.
-- Tipp 1: Hogyan van a program definiálva?
-- Tipp 2: Melyik típusosztály függvény válik hasznunkra?
evalProgram :: Program -> StateF Env String
evalProgram = undefined

