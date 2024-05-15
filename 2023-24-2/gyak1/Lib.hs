{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Lib where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.IO.Class
import Data.List
import Data.Bifunctor
import Data.Bitraversable
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import GHC.Stack
import Debug.Trace

{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}


-- Utils

stackTrace :: HasCallStack => String
stackTrace = concatMap
  (\(fun, s) -> "\tcall to '" ++ fun ++ "' at line " ++ show (srcLocStartLine s) ++ ", column " ++ show (srcLocStartCol s) ++ "\n") $
  getCallStack callStack

printRest :: Parser ()
printRest = get >>= traceM

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

runProgramT :: Monad m => [Statement] -> m (Either InterpreterError Env)
runProgramT = runExceptT . flip execStateT [] . evalProgram

runProgram :: [Statement] -> Either InterpreterError Env
runProgram = runExcept . flip execStateT [] . evalProgram

runProgramPretty :: [Statement] -> IO ()
runProgramPretty sts = do
  res <- runProgramT sts
  case res of
    Right env -> forM_ env $ \(var, val) -> putStrLn $ var ++ " == " ++ show val
    Left err -> putStrLn (message err)

parseAndRunProgram :: String -> IO ()
parseAndRunProgram s = do
  Right r <- bitraverse fail pure (parseProgram s)
  runProgramPretty r

run :: String -> Env
run s = case parseProgram s of
  Right sts -> case runProgram sts of
    Right e -> e
    _ -> error "interpreter error"
  _ -> error "parse error"

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
eof = get >>= \s -> (<|> throwError ("eof: String not empty. Remaining string: "  ++ s)) (guard $ null s)

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
  | Sign Exp             -- sign e
  deriving (Eq, Show)

instance Num Exp where
  (+) = (:+)
  (*) = (:*)
  abs x = x * signum x
  (-) = (:-)
  fromInteger = IntLit . fromInteger
  signum = Sign

instance Fractional Exp where
  (/) = (:/)
  fromRational = FloatLit . fromRational

{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| not, sign          | Prefix             | 20                 |
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
keywords = ["true", "false", "not", "sign", "if", "then", "do", "for", "lam", "end"]

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
pNot = (Not <$> (pKeyword "not" *> pNot)) <|> (Sign <$> (pKeyword "sign" *> pNot)) <|> pAtom

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
  deriving Show

program :: Parser [Statement]
program = sepBy statement (char' ';')

statement :: Parser Statement
statement = asum [sIf, sWhile, sAssign]

sIf :: Parser Statement
sIf = If <$> (pKeyword "if" *> pExp) <*> (pKeyword "then" *> program <* pKeyword "end")

sWhile :: Parser Statement
sWhile = While <$> (pKeyword "while" *> pExp) <*> (pKeyword "do" *> program <* pKeyword "end")

sAssign :: Parser Statement
sAssign = Assign <$> pNonKeyword <*> (pKeyword ":=" *> pExp)

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
  deriving Show

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError { message :: String } -- típushiba üzenettel
  | ScopeError { message :: String } -- variable not in scope üzenettel
  | DivByZeroError { message :: String } -- 0-val való osztás hibaüzenettel
  deriving Show

-- Értékeljünk ki egy kifejezést!
evalExp :: (HasCallStack, MonadError InterpreterError m) => Exp -> Env -> m Val
evalExp exp env = case exp of
  IntLit i -> return (VInt i)
  FloatLit f -> return (VFloat f)
  BoolLit b -> return (VBool b)
  LamLit s e -> return (VLam s env e)
  Not e -> evalExp e env >>= \case
    VBool b -> return (VBool $ not b)
    _       -> throwError (TypeError $ "Type error in the operand of not\nSTACK TRACE:\n" ++ stackTrace)
  Sign e -> evalExp e env >>= \case
    VInt i -> return (VInt $ signum i)
    VFloat f -> return (VFloat $ signum f)
    _       -> throwError (TypeError $ "Type error in the operand of sign\nSTACK TRACE:\n" ++ stackTrace)
  Var str -> case lookup str env of
    Just v -> return v
    Nothing -> throwError (ScopeError $ "Variable not in scope: " ++ str ++ "\nSTACK TRACE:\n" ++ stackTrace)
  e1 :+ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 + i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 + f2))
      _ -> throwError (TypeError $ "Type error in the operands of +\nSTACK TRACE:\n" ++ stackTrace)
  e1 :- e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 - i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 - f2))
      _ -> throwError (TypeError $ "Type error in the operands of -\nSTACK TRACE:\n" ++ stackTrace)
  e1 :* e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VInt (i1 * i2))
      (VFloat f1, VFloat f2) -> return (VFloat (f1 * f2))
      _ -> throwError (TypeError $ "Type error in the operands of *\nSTACK TRACE:\n" ++ stackTrace)
  e1 :/ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt 0) -> throwError (DivByZeroError $ "Cannot divide by integer zero\nSTACK TRACE:\n" ++ stackTrace)
      (VInt i1, VInt i2) -> return (VInt (div i1 i2))
      (VFloat f1, VFloat f2) | abs f2 < 0.0001 -> throwError (DivByZeroError $ "Cannot divide by float zero\nSTACK TRACE:\n" ++ stackTrace)
      (VFloat f1, VFloat f2) -> return (VFloat (f1 / f2))
      _ -> throwError (TypeError $ "Type error in the operands of /\nSTACK TRACE:\n" ++ stackTrace)
  e1 :== e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case (v1, v2) of
      (VInt i1, VInt i2) -> return (VBool (i1 == i2))
      (VFloat f1, VFloat f2) -> return (VBool (f1 == f2))
      (VBool b1, VBool b2) -> return (VBool (b1 == b2))
      _ -> throwError (TypeError $ "Type error in the operands of ==\nSTACK TRACE:\n" ++ stackTrace)
  e1 :$ e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    case v1 of
      (VLam s env' e) -> evalExp e ((s, v2) : env')
      _ -> throwError (TypeError $ "Type error in the operands of function application\nSTACK TRACE:\n" ++ stackTrace)


updateEnv :: Env -> String -> Val -> Env
updateEnv [] s v = [(s,v)]
updateEnv ((s', v'):xs) s v
  | s == s' = (s, v) : xs
  | otherwise = (s', v') : updateEnv xs s v

inBlockScope :: MonadState Env m => m a -> m a
inBlockScope f = do
  env <- get
  a <- f
  modify (take (length env))
  pure a

-- Állítás kiértékelésénér egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (HasCallStack, MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement st = case st of
  Assign x e -> do
    env <- get
    v <- evalExp e env
    put (updateEnv env x v)
  If e sts -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> inBlockScope $ evalProgram sts
      VBool _ -> pure ()
      _ -> throwError (TypeError $ "Type error in the condition of 'if'\nSTACK TRACE:\n" ++ stackTrace)
  While e sts -> do
    env <- get
    v1 <- evalExp e env
    case v1 of
      VBool True -> do
        inBlockScope $ evalProgram sts
        evalStatement (While e sts)
      VBool _ -> pure ()
      _ -> throwError (TypeError $ "Type error in the condition of 'while'\nSTACK TRACE:\n" ++ stackTrace)

--- PÉLDA PROGRAMOK

p1 :: String
p1 = concat [
  "x := 1;",
  "if (x == 1) then " ++ concat [
      "y := 2;",
      "x := 10 - y;"
   ] ++ "end"
            ]
p2 :: String
p2 = concat [
  "func := lam x -> x + 1;",
  "while (not ((func $ 0) == 1024)) do " ++ concat [
      "func := lam x -> (func $ x) * 2;"
  ] ++ "end;",
  "x := func $ 10"
            ]
