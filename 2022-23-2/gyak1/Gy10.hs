{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

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

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$>  some digit

integer :: Parser Int
integer = do
  sign <- optional $ char '-'
  n <- natural
  case sign of
    Nothing -> pure n
    Just _  -> pure $ negate n

between :: Parser left -> Parser a -> Parser right -> Parser a
between left a right = do -- left *> a <* right
  left
  a' <- a
  a' <$ right

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 a delim = (:) <$> a <*> many (delim *> a)

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy a delim = sepBy1 a delim <|> pure []

ws :: Parser ()
ws = void $ many $ satisfy isSpace

tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f a sep = foldr1 f <$> sepBy1 a sep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f a sep = foldl1 f <$> sepBy1 a sep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

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


data Exp = Or Exp Exp | And Exp Exp | Add Exp Exp | Mul Exp Exp | IntLit Int | Var String | BoolLit Bool deriving (Eq, Show)

pBool :: Parser Exp
pBool = BoolLit <$> (True <$ string' "true" <|> False <$ string' "false")

pAtom :: Parser Exp
pAtom = (IntLit <$> natural') <|> pBool <|> (Var <$> some (satisfy isLetter) <* ws) <|> between (char' '(') pAt (char' ')')

pMul :: Parser Exp
pMul = chainl1 pHash (Mul <$ char' '*')

pAdd :: Parser Exp
pAdd = chainl1 pMul (Add <$ char' '+')

pAt :: Parser Exp
pAt = chainl1 pAdd (And <$ char' '|')

pHash :: Parser Exp
pHash = chainr1 pAtom (Or <$ char' '&')

-- Egészítsük ki a nyelvet
-- negálás (prefix) művelettel
-- bool literálokkal
-- == műveletel (nem asszociáló)

-- Állítások
-- pl.: értékadás, control flow


data Statement
 = Assign String Exp -- e := kif
 | If Exp [Statement]  -- if kif then p₁ end
 | While Exp [Statement] -- while kif do p₁ end

keywords :: [String]
keywords = [ "for", "if", "do", "end", "true", "false" ]

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pKeyword' :: String -> Parser ()
pKeyword' s = do
  string s
  -- ha folytatódik az input betűvel, akkor fail
  -- egyébként pedig ws-t olvasunk és sikeresen visszatérünk
  (satisfy isLetter *> empty) <|> ws

-- Program parser
-- Legyenek ;-el elválasztva
program :: Parser [Statement]
program = sepBy1  statement (char' ';')

statement :: Parser Statement
statement = (Assign <$> pIdent' <*> (string' ":=" *> (pAdd <* ws)))
  <|> If <$> (pKeyword' "if" *> (pAdd <* ws)) <*> (pKeyword' "then" *> program) <* pKeyword' "end"
  <|> While <$> (pKeyword' "while" *> (pAdd <* ws)) <*> (pKeyword' "do" *> program) <* pKeyword' "end"

-- Eredményes típusa:
data Val = VInt Int | VBool Bool
  deriving (Eq, Show)

-- Környezet: egy változó mivel egyenlő
type Env = [(String, Val)]

-- Környezetbe beszúrás függvény
updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((y, v') : env)
  | x == y    = (x, v) : env
  | otherwise = (y, v') : updateEnv x v env


-- Kifejezés kiértékelése
evalExp :: Env -> Exp -> Val
evalExp env exp = case exp of
  Mul exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
    (VInt a, VInt b) -> VInt (a * b)
    _                -> error "Type mismatch"
  Add exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
    (VInt a, VInt b) -> VInt (a + b)
    _                -> error "Type mismatch"
  Or exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
    (VBool a, VBool b) -> VBool (a || b)
    _                -> error "Type mismatch"
  And exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
    (VBool a, VBool b) -> VBool (a && b)
    _                -> error "Type mismatch"
  IntLit i -> VInt i
  BoolLit b -> VBool b
  Var string -> case lookup string env of
    Just v -> v
    Nothing -> error "Variable not in scope"

-- State

newtype State s a = State { runState :: s -> (a,s) } deriving Functor

instance Applicative (State s) where
  pure a = State $ \s -> (a,s)
  (<*>) = ap

instance Monad (State s) where
  (State st) >>= f = State $ \s -> let (a, s') = st s in runState (f a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f


-- Értékeljünk ki egy állítást!
-- Tároljuk state-ben az Env-et
evalStatement :: Statement -> State Env ()
evalStatement st = case st of
  If exp prog -> do
    env <- get
    case evalExp env exp of
      (VBool True) -> inNewScope $ mapM_ evalStatement prog
      _            -> pure ()
  While exp prog -> while exp prog
  Assign nam exp -> do
    env <- get
    put (updateEnv nam (evalExp env exp) env)

while :: Exp -> [Statement] -> State Env ()
while cond sts = do
  env <- get
  case evalExp env cond of
    (VBool True) -> inNewScope $ mapM_ evalStatement sts *> while cond sts
    _            -> pure ()


evalProgramm :: [Statement] -> Env
evalProgramm st = case runState (mapM_ evalStatement st) [] of
  (_, env) -> env

-- Ha valami newScope-ban fut, akkor a futás után az újonnan felvett változókat
-- eldobjuk az Env-ből.
inNewScope :: State Env a -> State Env a
inNewScope ma = do
  l <- length <$> get
  a <- ma
  modify (take l)
  pure a
