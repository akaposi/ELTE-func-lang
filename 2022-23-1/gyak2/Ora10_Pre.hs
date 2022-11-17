{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs #-}
module Ora10 where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import Data.List

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) } deriving Functor

instance Applicative Parser where
    (<*>) = ap
    pure a = Parser $ \s -> Just (s,a)

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \s -> pa s >>= uncurry (flip (runParser . f))

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser pa) <|> (Parser pb) = Parser $ \s -> pa s <|> pb s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (xs, x) else Nothing

char :: Char -> Parser ()
char c = void $ satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (\x -> True)

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ([], ())
    _ -> Nothing

ws :: Parser ()
ws = void $ many $ satisfy isSpace

tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> p <* eof

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy pa pdelim = sepBy1 pa pdelim <|> pure []

sepBy1 :: Parser a -> Parser delim -> Parser [a]
sepBy1 pa pdelim = do
    a <- pa
    delim <- optional $ pdelim
    case delim of
        Just _ -> (\as -> a : as) <$> sepBy1 pa pdelim
        Nothing -> return [a]

digit :: Parser Int
digit = (\a -> ord a - ord '0') <$> satisfy isDigit

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> some digit

integer :: Parser Int
integer = do
    l <- optional $ char '-'
    case l of
        Just _ -> negate <$> natural
        _      -> natural

string :: String -> Parser ()
string = mapM_ char

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f a sep = do
    a' <- a
    sep' <- optional $ sep
    case sep' of
        Just _ -> (f a') <$> a
        Nothing -> pure a'


chainr1 :: Parser a->  Parser (a -> a -> a) ->  Parser a
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

chain1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chain1 a sep = do
    a' <- a
    sep' <- optional $ sep
    case sep' of
        Just f -> (f a') <$> a <* (optional sep >>= (\a -> case a of
            Just _ -> empty
            _      -> pure ()))
        Nothing -> pure a'

newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
    (<*>) = ap
    pure = State . flip (,)

instance Monad (State s) where
    (State a) >>= f = State $ \s -> let (s', a') = a s in runState (f a') s'


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put = State . const . flip (,) ()

modify :: (s -> s) -> State s ()
modify f = get >>= put . f


-- Kifejezésnyelv
data Exp
    = Var String
    | IntLit Int
    | BoolLit Bool
    | Leq Exp Exp -- <=
    | Lt Exp Exp -- <
    | Eq Exp Exp -- ==
    | Add Exp Exp
    | Mul Exp Exp
    | Pow Exp Exp
    deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "end", "true", "false"]

nonKeyword :: Parser String
nonKeyword = do
    s <- tok $ some (satisfy isLetter)
    if elem s reservedKeywords then empty else pure s

keyword' :: String -> Parser ()
keyword' s = do
    tok $ forM_ s char
    (satisfy isLetter >> empty) <|> ws

pAtom :: Parser Exp
pAtom = (Var <$> nonKeyword)
        <|> (IntLit <$> tok integer)
        <|> (BoolLit True <$ tok (keyword' "true"))
        <|> (BoolLit False <$ tok (keyword' "false"))
        <|> between (tok (char '(')) pBools (tok (char ')'))

pPow :: Parser Exp
pPow = chainr1 pAtom (Pow <$ tok (char '^'))

pMul :: Parser Exp
pMul = chainl1 pPow (Mul <$ tok (char '*'))

pAdd :: Parser Exp
pAdd = chainl1 pMul (Add <$ tok (char '+'))

pBools :: Parser Exp
pBools = chain1 pAdd (Eq <$ tok (string "==") <|> Leq <$ tok (string "<=") <|> Lt <$ tok (char '<'))

pExp :: Parser Exp
pExp = ws *> pBools

{-
Programozási nyelveknek két "rétege" van:
    Kifejezések (amilyen értékeket pl egy változó felvehet), ez az Exp adattípus
    Állítások (Nyelvi konstrukciók, elágazások, ciklusok, értékadások)

    A nyelvben amit modellezni fogunk az alábbi állítások lesznek:
    - Értékadás
    - Elágazás
    - While-Ciklus
-}

data Statement
    = Assign String Exp
    | IfThenElse Exp [Statement] [Statement]
    | While Exp [Statement]
    deriving (Eq, Show)

-- IfThenElse és While ágain belül lehet több ilyen állítás is, ezért lista a paramétere
-- Mellékhatásos függvényeket pl IO műveletek nem veszünk

{-
if állítás formája:

if (<exp>) then <[statement]> else <[statement]> end

assign állítás formája:

<string> := <exp>

while állítás formája:

while <exp> do <[statement]> end

Az állítások ;-el legyenek elválasztva
-}

program :: Parser [Statement]
program = sepBy1 (pAssign <|> pITE <|> pWhile) (tok $ char ';')

pAssign :: Parser Statement
pAssign = Assign <$> nonKeyword <*> (tok (string ":=") *> pExp)

pITE :: Parser Statement
pITE = IfThenElse <$> (keyword' "if" *> pExp <* keyword' "then") <*> (program <* keyword' "else") <*> (program <* keyword' "end")

pWhile :: Parser Statement
pWhile = While <$> (keyword' "while" *> pExp <* keyword' "do") <*> (program <* keyword' "end")

-- KIÉRTÉKELÉS

data Value = VInt Int | VBool Bool -- normalizált kifejezések
type Env = [(String, Value)] -- lookup table amibe a változók értékét tároljuk

-- A nyelvünkben minden kifejezés tiszta, nem okoznak állapotváltozást
-- Definiáljuk az evalExp függvényt ami kiértékel egy kifejezést!
-- Típushiba esetén dobjunk errort
evalExp :: Env -> Exp -> Value
evalExp = undefined

-- type Interpreter a = State Env a

-- Egy állítás kiértékelése okozhat állapotváltozsát, ezért az eredmény valami állapotváltozást lesz (State)
-- Nem kell extra env paraméter, mert benne van a stateben
-- Definiáljuk az evalStatement függvényt ami kiértékel egy állítást!
evalStatement :: Statement -> State Env ()
evalStatement = undefined

evalProgram :: [Statement] -> Env
evalProgram ss = fst $ runState (mapM_ evalStatement ss) []


-- Az egész interpreter ennyi, kész.
-- Egészítsük ki a nyelvet!
{-
    - >, >=, /= operátorokkal
    - %, -, / operátorokkal
    - &&, || operátorokkal
    - negálással (not kulcsszó)
    - for ciklussal
    - Tuple típussal és annak destruktoraival (fst, snd) <-- ! előadás
    - 
-}

-- Nem láncolható prefix parser
-- A prefix kifejezések az atom és az utolsó parser közé megy
pPrefix :: Parser a -> Parser (a -> a) -> Parser a
pPrefix a op = (op <*> a) <|> a
