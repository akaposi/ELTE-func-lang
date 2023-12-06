{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Traversable

{-# ANN module "HLint: ignore Use lambda-case" #-}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)} deriving (Functor)

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
  _ -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (x : xs) | p x -> Just (x, xs)
  _ -> Nothing

anyChar :: Parser Char
anyChar = satisfy $ const True

char :: Char -> Parser ()
char c = void $ satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

string :: String -> Parser ()
string = mapM_ char

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> some digit

integer :: Parser Int
integer = do
  sign <- optional $ char '-'
  n <- natural
  case sign of
    Nothing -> pure n
    Just _ -> pure $ negate n

between :: Parser left -> Parser a -> Parser right -> Parser a
between left a right = do
  -- left *> a <* right
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
    [e] -> pure e
    [e1, e2] -> pure (f e1 e2)
    _ -> empty

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
  val <- v
  ( do
      opr <- op
      res <- chainr1 v op
      pure (opr val res)
    )
    <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
  where
    parseLeft val =
      ( do
          opr <- op
          val2 <- v
          parseLeft (opr val val2)
      )
        <|> pure val

data Exp = Add Exp Exp | Mul Exp Exp | Neg Exp | IntLit Int | Eq Exp Exp | Sub Exp Exp | Var String | BoolLit Bool deriving (Eq, Show)

---           V         V            V
pPrefix :: (a -> a) -> Parser a -> Parser op -> Parser a
pPrefix f a op =
  ( do
      op
      a' <- a
      pure (f a')
  )
    <|> a

pAtom :: Parser Exp
pAtom = (Var <$> pIdent') <|> (IntLit <$> natural') <|> (BoolLit True <$ pKeyword' "true") <|> (BoolLit False <$ pKeyword' "false") <|> between (char' '(') pAdd (char' ')')

pNeg :: Parser Exp
pNeg = pPrefix Neg pAtom (char' '¬')

pMul :: Parser Exp
pMul = chainl1 pNeg (Mul <$ char' '*')

pEq :: Parser Exp
pEq = nonAssoc Eq pMul (string' "==")

pAdd :: Parser Exp
pAdd = chainl1 pEq (Add <$ char' '+' <|> Sub <$ char' '-')

-- Egészítsük ki a nyelvet
-- negálás (prefix) művelettel
-- bool literálokkal
-- == műveletel (nem asszociáló)

-- Állítások
-- pl.: értékadás, control flow

data Statement
  = Assign String Exp -- e := kif
  | If Exp [Statement] -- if kif do p₁ end
  | For Exp [Statement] -- for kif do p₁ end
  deriving (Eq, Show)

keywords :: [String]
keywords = ["for", "if", "do", "end", "true", "false"]

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLetter) <* ws
  if x `elem` keywords
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
program = sepBy1 statement (char' ';')

statement :: Parser Statement
statement =
  (Assign <$> (pIdent' <* string' ":=") <*> pAdd)
    <|> (If <$> (pKeyword' "if" *> pAdd) <*> (pKeyword' "do" *> program <* pKeyword' "end"))
    <|> (For <$> (pKeyword' "for" *> pAdd) <*> (pKeyword' "do" *> program <* pKeyword' "end"))

-- Eredményes típusa:
data Val = VInt Int | VBool Bool
  deriving (Eq, Show)

-- Környezet: egy változó mivel egyenlő
type Env = [(String, Val)]

-- Környezetbe beszúrás függvény
updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((y, v') : env)
  | x == y = (x, v) : env
  | otherwise = (y, v') : updateEnv x v env

-- Kifejezés kiértékelése
-- Add Exp Exp | Mul Exp Exp | Neg Exp | IntLit Int | Eq Exp Exp | Sub Exp Exp | Var String | BoolLit Bool deriving (Eq, Show)

evalExp :: Env -> Exp -> Val
evalExp env (Add x1 x2) = case (evalExp env x1, evalExp env x2) of
  (VInt i1, VInt i2) -> VInt (i1 + i2)
  (_, _) -> error "uhoh"
evalExp env (Mul x1 x2) = case (evalExp env x1, evalExp env x2) of
  (VInt i1, VInt i2) -> VInt (i1 * i2)
  (_, _) -> error "uhoh"
evalExp env (Neg x1) = case evalExp env x1 of
  VBool b -> VBool (not b)
  _ -> error "uhoh"
evalExp env (Sub x1 x2) = case (evalExp env x1, evalExp env x2) of
  (VInt i1, VInt i2) -> VInt (i1 - i2)
  (_, _) -> error "uhoh"
evalExp env (Eq x1 x2) = case (evalExp env x1, evalExp env x2) of
  (VInt i1, VInt i2) -> VBool (i1 == i2)
  (VBool b1, VBool b2) -> VBool (b1 == b2)
  (_, _) -> error "uhoh"
evalExp env (IntLit i) = VInt i
evalExp env (BoolLit b) = VBool b
evalExp env (Var n) = case lookup n env of
  Just v -> v
  Nothing -> error "variable not in scope"

-- State

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  (State st) >>= f = State $ \s -> let (a, s') = st s in runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

-- Értékeljünk ki egy állítást!
-- Tároljuk state-ben az Env-et
evalStatement :: Statement -> State Env ()
evalStatement (If p pr) = do
  env <- get
  case evalExp env p of
    VBool True -> inNewScope $ evalProgram pr
    VBool False -> pure ()
    _ -> error "uhoh"
evalStatement (For p pr) = do
  env <- get
  case evalExp env p of
    VBool True -> inNewScope $ evalProgram pr >> evalStatement (For p pr)
    VBool False -> pure ()
    _ -> error "uhoh"
evalStatement (Assign x e) = do
  env <- get
  modify (updateEnv x (evalExp env e))

evalProgram :: [Statement] -> State Env ()
evalProgram = mapM_ evalStatement

-- Ha valami newScope-ban fut, akkor a futás után az újonnan felvett változókat
-- eldobjuk az Env-ből.
inNewScope :: State Env a -> State Env a
inNewScope ma = do
  l <- length <$> get
  a <- ma
  modify (take l)
  pure a
