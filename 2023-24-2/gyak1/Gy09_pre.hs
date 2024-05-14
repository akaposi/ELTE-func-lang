{-# LANGUAGE LambdaCase #-}
module Gy09 where

import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable

-- Parser

type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitívek

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> (some (digitToInt <$> satisfy isDigit) <|> throwError "natural: number had no digits")

integer :: Parser Int
integer = maybe id (const negate) <$> optional (char '-') <*> natural

float :: Parser Double
float = do
    s <- maybe id (const negate) <$> optional (char '-')
    i <- natural
    char '.' <|> throwError "float: No digit separator"
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s (r / 10 + fromIntegral i)

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 p delim = (:) <$> (p <|> throwError "sepBy1: no elements")
                     <*> ((delim *> sepBy p delim) <|> pure [])

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p delim = sepBy1 p delim <|> pure []

-- Whitespace-k elhagyása
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenizálás: whitespace-ek elhagyása
tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- A tokenizált parsereket '-al szoktuk jelölni

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

float' :: Parser Double
float' = tok float

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f p sep = chainr1 p (f <$ sep)

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f p sep = chainl1 p (f <$ sep)

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e] -> pure e
    [e1, e2] -> pure (f e1 e2)
    _ -> throwError "nonAssoc: too many or too few associations"

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

-- Kifejezésnyelv
data Exp
  = IntLit Int           -- 1 2 ...
  | FloatLit Double      -- 1.0 2.11 ...
  | BoolLit Bool         -- true false
  | Var String           -- x y ...
  | LamLit String Exp    -- \x -> e
  | Exp :+ Exp           -- e1 + e2
  | Exp :* Exp           -- e1 * e2
  | Exp :- Exp           -- e1 - e2
  | Exp :/ Exp           -- e1 / e2
  | Exp :== Exp          -- e1 == e2
  | Exp :$ Exp           -- e1 $ e2
  | Not Exp              -- not e
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| not                | Prefix             | 20                 |
+--------------------+--------------------+--------------------+
| *                  | Jobbra             | 18                 |
+--------------------+--------------------+--------------------+
| /                  | Balra              | 16                 |
+--------------------+--------------------+--------------------+
| +                  | Jobbra             | 14                 |
+--------------------+--------------------+--------------------+
| -                  | Balra              | 12                 |
+--------------------+--------------------+--------------------+
| ==                 | Nincs              | 10                 |
+--------------------+--------------------+--------------------+
| $                  | Jobbra             | 8                  |
+--------------------+--------------------+--------------------+

-}

keywords :: [String]
keywords = ["true", "false", "not"]

pNonKeyword :: Parser String
pNonKeyword = do
  res <- tok $ some (satisfy isLetter)
  res <$ (guard (res `notElem` keywords) <|> throwError "pNonKeyword: parsed a keyword")

pKeyword :: String -> Parser ()
pKeyword = string'

pAtom :: Parser Exp
pAtom = asum [
  FloatLit <$> float',
  IntLit <$> integer',
  BoolLit True <$ pKeyword "true",
  BoolLit False <$ pKeyword "false",
  LamLit <$> (pKeyword "lam" *> pNonKeyword) <*> (string' "->" *> pExp),
  Var <$> pNonKeyword,
  between (char' '(') pExp (char' ')')
             ] <|> throwError "pAtom: no literal, var or bracketed matches"

pNot :: Parser Exp
pNot = (Not <$> (pKeyword "not" *> pNot)) <|> pAtom

pMul :: Parser Exp
pMul = chainr1 pNot ((:*) <$ char' '*')

pDiv :: Parser Exp
pDiv = chainl1 pMul ((:/) <$ char' '/')

pAdd :: Parser Exp
pAdd = chainr1 pDiv ((:+) <$ char' '+')

pMinus :: Parser Exp
pMinus = chainl1 pAdd ((:-) <$ char' '-')

pEq :: Parser Exp
pEq = nonAssoc (:==) pMinus (string' "==")

pDollar :: Parser Exp
pDollar = chainr1 pEq ((:$) <$ char' '$')

pExp :: Parser Exp -- táblázat legalja
pExp = pDollar

-- Állítások: értékadás, elágazások, ciklusok
data Statement
  = If Exp [Statement]        -- if e then p end
  | While Exp [Statement]     -- while e do p end
  | Assign String Exp         -- v := e
  | Label String [Statement]  -- v: p
  | GoTo String               -- goto v
  | Return Exp

-- Írjunk ezekre parsereket!
-- Egy programkód egyes sorait ;-vel választjuk el

program :: Parser [Statement]
program = concat <$> sepBy (sAssigns <|> singleton <$> statement) (char' ';')

statement :: Parser Statement
statement = asum [
  sIf,
  sWhile,
  sLabel,
  sAssign,
  sGoto,
  sReturn
  ] <|> throwError "statement: No valid statements found!"

sIf :: Parser Statement
sIf = string "if" *> (If <$> (pExp <* string' "then") <*> (program <* string' "end"))

sWhile :: Parser Statement
sWhile = string "while" *> (While <$> (pExp <* string' "do") <*> (program <* string' "end"))

sAssign :: Parser Statement
sAssign = Assign <$> (pNonKeyword <* string' ":=") <*> pExp

sAssigns :: Parser [Statement]
sAssigns = do
  vars <- sepBy1 pNonKeyword (char' ',')
  string' ":="
  exps <- sepBy1 pExp (char' ',')
  if length vars /= length exps then
    throwError "sAssings: Not enough variable names or values!"
  else
    pure $ zipWith Assign vars exps

sLabel :: Parser Statement
sLabel = Label <$> (pNonKeyword <* char ':') <*> program

sGoto :: Parser Statement
sGoto = GoTo <$> (string' "goto" *> pNonKeyword)

sReturn :: Parser Statement
sReturn = string' "return" *> (Return <$> pExp)

parseProgram :: String -> Either String [Statement]
parseProgram s = case runParser (topLevel program) s of
  Left e -> Left e
  Right (x,_) -> Right x

-- Interpreter
-- Kiértékelt értékek típusa:
data Val
  = VInt Int              -- int kiértékelt alakban
  | VFloat Double         -- double kiértékelt alakban
  | VBool Bool            -- bool kiértékelt alakban
  | VLam String Env Exp   -- lam kiértékelt alakban
  deriving (Show)

instance Eq Val where
  (==) :: Val -> Val -> Bool
  (==) (VInt i) (VInt j) = i == j
  (==) (VFloat i) (VFloat j) = i == j
  (==) (VBool b) (VBool bb) = b == bb
  (==) _ _ = False
  

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError String -- típushiba üzenettel
  | ScopeError String -- variable not in scope üzenettel
  | DivByZeroError String -- 0-val való osztás hibaüzenettel
  deriving (Show)

-- Az interpreter típusát nem adjuk meg explicit, hanem használjuk a monád transzformerek megkötéseit!
-- Értékeljünk ki egy kifejezést!
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp (IntLit a) _ = pure $ VInt a
evalExp (FloatLit f) _ = pure $ VFloat f
evalExp (BoolLit b) _ = pure $ VBool b
evalExp (LamLit s e) env = pure $ VLam s env e
evalExp (Var a) env = case lookup a env of
  Just a -> pure a
  Nothing -> throwError $ ScopeError ("Can not find variable in scope: " ++ a)
evalExp (Not e) env = (evalExp e env) >>= \b -> case b of
  (VBool b) -> pure $ VBool (not b)
  _ -> throwError $ TypeError "Can not 'not' non-bool values!"
evalExp (e1 :+ e2) env = evalExp e1 env >>= \v1 -> evalExp e2 env >>= \v2 -> case (v1, v2) of
  (VInt a , VInt b) -> pure $ VInt (a + b)
  (VFloat f , VFloat g) -> pure $ VFloat (f + g)
  (VInt a , VFloat g) -> pure $ VFloat ((fromIntegral a) + g)
  (VFloat f, VInt b) -> pure $ VFloat (f + (fromIntegral b))
  _ -> throwError $ TypeError "Addition only works on numbers!"
evalExp (e1 :- e2) env = evalExp e1 env >>= \v1 -> evalExp e2 env >>= \v2 -> case (v1, v2) of
  (VInt a , VInt b) -> pure $ VInt (a - b)
  (VFloat f , VFloat g) -> pure $ VFloat (f - g)
  (VInt a , VFloat g) -> pure $ VFloat ((fromIntegral a) - g)
  (VFloat f, VInt b) -> pure $ VFloat (f - (fromIntegral b))
  _ -> throwError $ TypeError "Substraction only works on numbers!"
evalExp (e1 :* e2) env = evalExp e1 env >>= \v1 -> evalExp e2 env >>= \v2 -> case (v1, v2) of
  (VInt a , VInt b) -> pure $ VInt (a * b)
  (VFloat f , VFloat g) -> pure $ VFloat (f * g)
  (VInt a , VFloat g) -> pure $ VFloat ((fromIntegral a) * g)
  (VFloat f, VInt b) -> pure $ VFloat (f * (fromIntegral b))
  _ -> throwError $ TypeError "Multiplication only works on numbers!"
evalExp (e1 :/ e2) env = evalExp e1 env >>= \v1 -> evalExp e2 env >>= \v2 -> case (v1, v2) of
  (VInt _ , VInt 0) -> throwError $ DivByZeroError "Unable to divide by zero!"
  (VFloat _ , VInt 0) -> throwError $ DivByZeroError "Unable to divide by zero!"
  (VInt a , VInt b) -> pure $ VInt (a + b)
  (VFloat f , VFloat g) -> if (abs g <= 0.0000000000001) then (throwError $ DivByZeroError "Unable to divide by zero!") else pure $ VFloat (f + g)
  (VInt a , VFloat g) -> if (abs g <= 0.0000000000001) then (throwError $ DivByZeroError "Unable to divide by zero!") else pure $ VFloat ((fromIntegral a) + g)
  (VFloat f, VInt b) -> pure $ VFloat (f + (fromIntegral b))
  _ -> throwError $ TypeError "Addition only works on numbers!"
evalExp (e1 :== e2) env = VBool <$> ((==) <$> evalExp e1 env <*> evalExp e2 env)
evalExp (e1 :$ e2) env = evalExp e1 env >>= \lam -> case lam of
  (VLam v env e3) -> evalExp e2 env >>= \inp -> evalExp e3 ((v, inp) : env)
  _ -> throwError $ TypeError "Can not apply paramater to non-lambda expression!"


-- Állítás kiértékelésénér egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement (If e bo) = get >>= evalExp e >>= \b -> case b of
  (VBool b) -> get >>= \h -> if b then void $ traverse evalStatement bo else pure ()
  _ -> throwError $ TypeError "In if statement the expression must be a bool!"
evalStatement (While e bo) = get >>= evalExp e >>= \b -> case b of
  (VBool b) -> get >>= \h -> if b then void $ traverse evalStatement bo else pure ()
  _ -> throwError $ TypeError "In while statement the expression must be a bool!"
evalStatement (Assign s e) = get >>= \st -> evalExp e st >>= \v -> put ((s ,v) : st)
evalStatement (Label s st) = void $ traverse evalStatement st
evalStatement (GoTo s) = pure ()
evalStatement (Return e) = get >>= \st -> evalExp e st >>= \v -> put (("ans", v) : st)


evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

evalProgram' :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m (Maybe String)
evalProgram' s = let h = (mapM evalStatement s >>= \_ -> get) in (lookup "ans" <$> h) >>= \m -> pure $ (show <$> m)

-- Egészítsük ki a nyelvet egy print állítással (hint: MonadIO megkötés)
-- Egészítsük ki a nyelvet más típusokkal (tuple, either stb)

pe :: (MonadError InterpreterError m, MonadState Env m) => String -> m (Maybe String) 
pe str = case parseProgram str of
  (Left err)  -> throwError $ ScopeError err
  (Right val) -> evalProgram' val 

-- >>> :t evalStateT
-- evalStateT :: Monad m => StateT s m a -> s -> m a

-- >>> evalStateT ((pe "return 0") :: StateT Env (Either InterpreterError) (Maybe String)) []
-- Right (Just "VInt 0")
