{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy09 where

import Data.Functor
import Data.Foldable
import Data.List
import Data.Char
import Data.Traversable
import Control.Monad
import Control.Applicative

{-# ANN module "HLint: ignore Use lambda-case" #-}

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

instance Applicative Parser where
  (<*>) = ap
  pure a = Parser $ \s -> Just (a, s)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser p1) >>= f = Parser $ p1 >=> \(a, s') -> runParser (f a) s'


eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (x:xs) | p x -> Just (x, xs)
  _            -> Nothing

anyChar :: Parser Char
anyChar = satisfy $ const True

char :: Char -> Parser ()
char c = void $ satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

string :: String -> Parser ()
string = mapM_ char

-- Eredményes parserek: Olyan parserek amelyeknek van valami eredménye és fel is használjuk őket

-- Parseoljunk be legalább 1 számjegyet!
atLeastOneDigit :: Parser [Int]
atLeastOneDigit = some digit

-- Ennek segítségével tudunk már természetes számokat parseolni
natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> atLeastOneDigit
{-
natural = do
  digits <- atLeastOneDigit
  pure $ foldl1 (\acc a -> acc * 10 + a)
-}

-- Parseoljunk be egy egész számot! (Előjel opcionális)
integer :: Parser Int
integer = do
  sign <- optional $ char '-'
  n <- natural
  case sign of
    Nothing -> pure n
    Just _  -> pure $ negate n

-- Bónusz: Float parser (nem kell tudni, csak érdekes)
float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s * (r / 10 + fromIntegral i)

-- Definiáljunk egy parsert ami két adott parser között parseol valami mást
-- pl bewteen (char '(') (string "alma") (char ')') illeszkedik az "(alma)"-ra de nem az "alma"-ra
between :: Parser left -> Parser a -> Parser right -> Parser a
between left a right = do -- left *> a <* right
  left
  a' <- a
  a' <$ right





-- Definiáljunk egy parsert ami valami elválasztó karakterrel elválasztott parsereket parseol (legalább 1-et)
-- pl
-- runParser (sepBy1 anyChar (char ',')) "a,b,c,d" == Just ([a,b,c,d], "")
-- runParser (sepBy1 anyChar (char ',')) "a" == Just ([a], "")
-- runParser (sepBy1 anyChar (char ',')) "" == Nothing

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 a delim = do -- (:) <$> a <*> many (delim *> a)
  a' <- a
  as <- many (delim *> a)
  pure (a' : as)

-- Ugyanaz mint a fenti, de nem követeli meg, hogy legalább 1 legyen
sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy a delim = sepBy1 a delim <|> pure []

-- Írjunk egy parsert ami egy listaliterált parseol számokkal benne!
-- pl [1,2,30,40,-10]
listOfNumbers :: Parser [Int]
listOfNumbers = between (char '[') (sepBy integer (char ',')) (char ']')

-- Whitespace-k elhagyása
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenizálás: whitespace-ek elhagyása
tok :: Parser a -> Parser a
tok p = p <* ws -- Itt a <* kell mert a bal parser eredménye érdekes

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- A tokenizált parsereket '-al szoktuk jelölni

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

-- Írjuk újra a listOfNumbers parsert úgy, hogy engedjen space-eket a számok előtt és után illetve a [ ] előtt és után!
goodListofNumbers :: Parser [Int]
goodListofNumbers = topLevel $ between (char' '[') (sepBy integer' (char' ',')) (char' ']')

-- Hajtogató parserek

-- Az alábbi parserek kifejezésnyelvekhez lesznek a segédparsereink

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep

-- Nem kötelező HF
-- Olyan parser amit nem lehet láncolni (pl == mert 1 == 2 == 3 == 4 se jobbra se balra nem asszociál tehát nincs értelmezve)
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc = undefined

-- Kifejezésnyelvek

-- 1 * 2 + (3 * 4) + 5 + (6 + 7 * 8)

-- Az alábbi lesz a kifejezésnyelvünk
data Exp = Add Exp Exp | Mul Exp Exp | IntLit Int | Pow Exp Exp | Minus Exp Exp | Div Exp Exp | Rem Exp Exp deriving (Eq, Show)

-- Kiértékelő függvény:
evalExp :: Exp -> Int
evalExp (Add e1 e2) = evalExp e1 + evalExp e2
evalExp (Mul e1 e2) = evalExp e1 * evalExp e2
evalExp (IntLit i) = i

-- Recursive Descent Parsing algoritmus: https://en.wikipedia.org/wiki/Recursive_descent_parser
-- Minden precedencia szintnek külön parserje lesz
-- Lesz egy parser ami a legalja (atom)
-- Minden parser a fölötte lévő parser szintent használja majd
{-
A műveleteink:

    Pow | (^) | Infixr 9
    Mul | (*) | Infixl 7 <- fontos az irány (balra)
    Add | (+) | Infixl 6

Ez alapján a szintek:

        atom : Zárójelezést (zárójelben resetelődik a precedencia) és literálokat parse-ol
        mul : szorzást (és meghívja az atomot)
        add : összeadás (és meghívja a mult)
-}

-- ^ (Infixr 9)
-- - (Infixl 4)
-- % (Infixl 7)
-- / (Infixl 8)

pAtom :: Parser Exp
pAtom = (IntLit <$> integer') <|> (between (char' '(') pMinus (char' ')'))

pMul :: Parser Exp
pMul = chainl1 pExp (Mul <$ char' '*')
pAdd :: Parser Exp
pAdd = chainl1 pMul (Add <$ char' '+')--leftAssoc Add pMul (char' '+')

pMinus :: Parser Exp
pMinus = chainl1 pAdd (Minus <$ char' '-')--leftAssoc Minus pAdd (char' '-')

pExp :: Parser Exp
pExp = chainr1 pAtom (Pow <$ char' '^')--rightAssoc Pow pAtom (char' '^')

-- Írjunk parsert az alábbi nyelvekre!

data Exp' = IntLit' Int | Add' Exp' Exp' | Mul' Exp' Exp' | Var String deriving (Eq, Show)

evalExp' :: Exp' -> [(String, Exp')] -> Maybe Int
evalExp' (IntLit' n) _ = Just n
evalExp' (Add' e1 e2) lt = liftA2 (+) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Mul' e1 e2) lt = liftA2 (*) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Var str) lt = case lookup str lt of
    Just exp -> evalExp' exp lt
    _        -> Nothing

pLiteral :: Parser Exp'
pLiteral = do
  varname <- some $ satisfy isLetter
  pure $ Var varname

pAtom' :: Parser Exp'
pAtom' = (IntLit' <$> integer') <|> pLiteral <|> (between (char' '(') pAdd' (char' ')'))

pMul' :: Parser Exp'
pMul' = leftAssoc Mul' pAtom' (char' '*')

pAdd' :: Parser Exp'
pAdd' = leftAssoc Add' pMul' (char' '+')

data Exp'' = Add'' Exp'' Exp'' | Sub Exp'' Exp'' | Mul'' Exp'' Exp'' | IntLit'' Integer | Var' String deriving (Eq, Show)

evalExp'' :: Exp'' -> [(String, Exp'')] -> Maybe Integer
evalExp'' (IntLit'' n) _ = Just n
evalExp'' (Add'' e1 e2) lt = liftA2 (+) (evalExp'' e1 lt) (evalExp'' e2 lt)
evalExp'' (Mul'' e1 e2) lt = liftA2 (*) (evalExp'' e1 lt) (evalExp'' e2 lt)
evalExp'' (Var' str) lt = case lookup str lt of
    Just exp -> evalExp'' exp lt
    _        -> Nothing


---

-- chainr1 pMul (Add <$ char' '+' <|> Sub <$ char' '-')

chainr1 :: Parser a ->  Parser (a -> a -> a) ->  Parser a
chainr1 v op = do
    val <- v
    (do
        opr <- op
        res <- chainr1 v op
        pure (opr val res)
        ) <|> pure val

chainl1 :: Parser a ->  Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (do
            opr <- op
            val2 <- v
            parseLeft (opr val val2)) <|> pure val
