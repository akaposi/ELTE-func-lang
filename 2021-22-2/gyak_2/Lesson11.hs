module Lesson11 where

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
    
evalParser :: Parser a -> String -> Maybe a
evalParser p = (fst <$>) . runParser p

execParser :: Parser a -> String -> Maybe String
execParser p = (snd <$>) . runParser p
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
many' pa = some' pa <|> pure []

some' :: Parser a -> Parser [a]
some' pa = do
    a <- pa
    as <- many' pa
    pure (a:as)

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
between pOpen pa pClose = do
    pOpen
    a <- pa
    pClose
    pure a
-- Ezt általánosabban is meg lehet írni.

between' :: Parser open -> Parser a -> Parser close -> Parser a
between' pOpen pa pClose = pOpen *> pa <* pClose

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

inList :: [Char] -> Parser Char
inList [] = empty
inList (x:xs) = (x <$ char x) <|> inList xs

inList_ :: [Char] -> Parser ()
inList_ = void . inList

-- std függvény:
choice :: [Parser a] -> Parser a
choice []     = empty
choice (p:ps) = p <|> choice ps

lowercase :: Parser ()
lowercase = inList_ ['a'..'z']

digit_ :: Parser ()
digit_ = inList_ ['0'..'9']

digit :: Parser Integer
digit = fmap (\c -> fromIntegral (fromEnum c - fromEnum '0')) $ inList ['0'..'9']

   -- Functor/Applicative operátorok
   --   (<$)       kicseréli parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat balról jobbra, az első értékét visszaadja
   --   (*>)       két parser-t futtat balról jobbra, a második értékét visszaadja

integer :: Parser Integer
integer = do
    a <- optional $ char '-'
    digits <- some digit
    let summa = foldl (\acc x -> 10 * acc + x) 0 digits
    pure $ case a of
        Nothing -> summa
        Just _  -> -summa

ws :: Parser ()
ws = many_ (satisfy isSpace)

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
    a <- pa
    as <- many $ psep *> pa
    pure $ a:as

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-----------------------------------------------------------------------

mulOrAdd :: Parser (Integer -> Integer -> Integer)
mulOrAdd = do
    x <- optional $ char' '+'
    case x of
        Just _ -> pure (+)
        Nothing -> (*) <$ char' '*'

pAdd :: Parser Integer
pAdd = do
    ws
    a <- integer
    ws
    f <- mulOrAdd
    b <- integer
    ws
    (f a b) <$ eof

--------------------------------------------------------------------
-- Számok
-- + összeadás művelet
-- * szorzás művelet 
-- ()
-- zárójel a legerősebb, aztán *, aztán +
-- +,* balra kötő műveletek

data Exp = Nice | Lit Integer | Plus Exp Exp | Minus Exp Exp | Mul Exp Exp | Pow Exp Exp deriving Show

evalExp :: Exp -> Integer
evalExp Nice         = 69
evalExp (Lit n)      = n
evalExp (Plus e1 e2) = evalExp e1 + evalExp e2
evalExp (Mul e1 e2)  = evalExp e1 * evalExp e2
evalExp (Pow e1 e2)  = evalExp e1 ^ evalExp e2
evalExp (Minus e1 e2) = undefined -- ezzel egészítettem ki óra végén az Exp-et, de nem beszéltük meg, hogy mit kell csinálni, hogy a Plus-szal azonos precedencián lehessen beolvasni.

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

tokenize :: Parser a -> Parser a
tokenize pa = pa <* ws

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

-- 4 + 4 * 45 + (6 + 7) * 9
-- Plus (Plus (Lit 4) (Mul (Lit 4) (Lit 45))) (Mul (Plus (Lit 6) (Lit 7)) (Lit 9))
-- Lit :: Integer -> Exp

try :: Parser a -> Parser a
try pa = Parser $ \str -> case runParser pa str of
    Just (a,_) -> Just (a,str)
    Nothing    -> Nothing

pKeyword :: String -> Parser ()
pKeyword str = do
  string str
  mc <- optional (satisfy isDigit)
  case mc of
    Nothing -> ws
    Just _  -> empty

pBase :: Parser Exp -- számok ÉS zárójeles kifejezések
pBase = (Nice <$ pKeyword "69")
    <|> (Lit <$> tokenize integer)
    <|> tokenize (between (char' '(') pPlus (char' ')'))

pPow :: Parser Exp
pPow = rightAssoc Pow pBase (char' '^')

pMul :: Parser Exp
pMul = leftAssoc Mul pPow (char' '*')

pPlus :: Parser Exp
pPlus = leftAssoc Plus pMul (char' '+')

pExp :: Parser Exp
pExp = topLevel pPlus