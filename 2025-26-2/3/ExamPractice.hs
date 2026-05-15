{-# LANGUAGE LambdaCase #-}
module ExamPractice where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.Foldable
import Data.Bifunctor
import Data.Functor
import Data.Char
import Data.Foldable
import Data.Bitraversable


-- Data related tasks

data LengthIndexedList i a = Nil | Cons i a (LengthIndexedList i a) deriving (Eq, Show)

l1 :: LengthIndexedList Integer Char
l1 = Cons 11 'h' $ Cons 10 'e' $ Cons 9 'l' $ Cons 8 'l' $ Cons 7 'o' $ Cons 6 ' ' $ Cons 5 'w' $ Cons 4 'o' $ Cons 3 'r' $ Cons 2 'l' $ Cons 1 'd' $ Nil

l2 :: LengthIndexedList Int Bool
l2 = let l@(Cons i a r) = fmap not l2 in Cons (i + 1) True l

l3 :: LengthIndexedList Integer Int
l3 = Cons 2 2 $ Cons 1 1 Nil

myIndList :: LengthIndexedList Int Float
myIndList = Cons 2 (2.0) (Cons 1 (4.0) Nil)

instance Functor (LengthIndexedList i) where
    fmap :: (a -> b) -> LengthIndexedList i a -> LengthIndexedList i b 
    fmap f Nil = Nil 
    fmap f (Cons n a l) = Cons n (f a) (fmap f l)

instance Foldable (LengthIndexedList i) where
    foldr :: (a -> b -> b) -> b -> LengthIndexedList i a -> b
    foldr f b Nil = b 
    foldr f b (Cons n a l) = f a (foldr f b l)

instance Traversable (LengthIndexedList i) where
    traverse :: Applicative f => (a -> f b) -> LengthIndexedList i a -> f (LengthIndexedList i b)
    traverse g Nil = pure Nil
    traverse g (Cons n a l) = (Cons n <$> g a) <*> traverse g l

satisfyInvariant :: (Num i, Eq i) => LengthIndexedList i a -> Bool
satisfyInvariant (Cons n _ Nil) = n == 1
satisfyInvariant (Cons n _ (Cons m a l)) = n == m + 1 && satisfyInvariant (Cons m a l)
satisfyInvariant _ = True

mkLIL :: (Foldable f, Num i) => f a -> LengthIndexedList i a
mkLIL = undefined

reverseLIL :: Num i => LengthIndexedList i a -> LengthIndexedList i a
reverseLIL = undefined


-- Monad transformer exercises

runLawnmover lawnmoverMonad initialState = runWriterT $ runStateT lawnmoverMonad initialState

--mowAt :: (Int, Int) -> StateT ((Int, Int) -> Bool) (WriterT [(Int, Int)] IO) ()
--mowAt = undefined

-- Typeclass-style
type MonadMow m = (MonadState ((Int, Int) -> Bool) m, MonadWriter [(Int, Int)] m, MonadIO m)

mowAt :: MonadMow m => (Int, Int) -> m ()
mowAt coord = do 
    s <- get -- get state
    when (s coord) ( -- when there is grass on the tile
        do 
            tell [coord] -- update list of mowed tiles
            modify (\f ii -> ii /= coord && f ii)) -- update state to no grass on mowed tiles
--                 :: ((Int, Int) -> Bool) -> (Int, Int) -> Bool

-- Parser and interpreter tasks

-- Parser

type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

-- Run parser 0 or more times
many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

-- Run parser 1 or more times
some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitive parser combinators

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

sepBy1 :: Parser a -> Parser delim -> Parser {- not empty -} [a]
sepBy1 p delim = (:) <$> (p <|> throwError "sepBy1: no elements")
                     <*> ((delim *> sepBy p delim) <|> pure [])

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy p delim = sepBy1 p delim <|> pure []

-- Whitespace dropping
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenisation: dropping all whitespaces after a parser
tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- We label tokenized parsers with '

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

-- Expression language
data Exp
  = IntLit Int           -- 1 2 ...
  | FloatLit Double      -- 1.0 2.11 ...
  | BoolLit Bool         -- true false
  | Var String           -- x y ...
  | LamLit String Exp    -- lam x -> e
  | Exp :+ Exp           -- e1 + e2
  | Exp :* Exp           -- e1 * e2
  | Exp :- Exp           -- e1 - e2
  | Exp :/ Exp           -- e1 / e2
  | Exp :== Exp          -- e1 == e2
  | Exp :$ Exp           -- e1 $ e2
  | Not Exp              -- not e
  | ListLit [Exp]        -- NEW 
  | Exp :!! Exp          -- NEW
  deriving (Eq, Show)

{-
+--------------------+--------------------+--------------------+
| Operator name      | Direction          | Precedence         |
+--------------------+--------------------+--------------------+
| not                | Prefix             | 20                 |
+--------------------+--------------------+--------------------+
| *                  | Right              | 18                 |
+--------------------+--------------------+--------------------+
| /                  | Left               | 16                 |
+--------------------+--------------------+--------------------+
| +                  | Right              | 14                 |
+--------------------+--------------------+--------------------+
| -                  | Left               | 12                 |
+--------------------+--------------------+--------------------+
| ==                 | None               | 10                 |
+--------------------+--------------------+--------------------+
| $                  | Right              | 8                  |
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

pExp :: Parser Exp -- bottom of the table
pExp = pDollar

-- Statements: assigment, branching, loops
data Statement
  = If Exp [Statement]        -- if e then p end
  | While Exp [Statement]     -- while e do p end
  | Assign String Exp         -- v := e
  | AssignAt String Exp Exp   -- NEW
  deriving (Eq, Show)

-- Define parser for the statements above!
-- All statements in a program must be followed by a semicolon

program :: Parser [Statement]
program = sepBy1 statement (char' ';')

statement :: Parser Statement
statement = sIf <|> sWhile <|> sAssign

-- Alternative:
-- program = some statement
-- statement = (sIf <|> sWhile <|> sAssign) <* (char' ';')

sIf :: Parser Statement
sIf = do
  pKeyword "if"
  exp <- pExp
  pKeyword "then"
  stmts <- program 
  pKeyword "end"
  return $ If exp stmts

sWhile :: Parser Statement
sWhile = do
  pKeyword "while"
  exp <- pExp 
  pKeyword "do"
  stmts <- program 
  pKeyword "end"
  return $ While exp stmts

sAssign :: Parser Statement
sAssign = do
  varname <- pNonKeyword
  pKeyword ":="
  exp <- pExp 
  return $ Assign varname exp

parseProgram :: String -> Either String [Statement]
parseProgram s = case runParser (topLevel program) s of
  Left e -> Left e
  Right (x,_) -> Right x

-- Try running:
-- parseProgram "x := 1; x := x - 5;"
-- parseProgram "x := 1; b := false; if b == true then x := x * 2 end;"
-- parseProgram "x := 1; b := false; while b == false do x := x * 2; i := true; end;"

-- Interpreter
-- Type of evaluated expressions:
data Val
  = VInt Int              -- evaled int
  | VFloat Double         -- evaled double
  | VBool Bool            -- evaled bool
  | VLam String Env Exp   -- evaled lam, Env is the environment at the point the lambda was created (the closure)
  deriving (Eq, Show)

type Env = [(String, Val)] -- the evaluation environment

data InterpreterError
  = TypeError {msg :: String} -- type errors 
  | ScopeError {msg :: String} -- scope errors
  | DivByZeroError {msg :: String} -- division by zero errors
  deriving (Eq, Show)

-- We don't explicitly give the type of the interpreter monad, but use constraints instead
-- Let's evaluate expressions!
-- Note: evalExp cannot modify the Env
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp e env = case e of 
  IntLit x -> return $ VInt x
  FloatLit x -> return $ VFloat x
  BoolLit x -> return $ VBool x 
  Var str -> case lookup str env of
    Nothing -> throwError $ ScopeError ("Variable " ++ str ++ " not in scope")
    Just val -> return val
  LamLit str exp -> return $ VLam str env exp  
  x :+ y -> do
    xv <- evalExp x env
    yv <- evalExp y env
    case (xv, yv) of
      (VInt xx, VInt yy) -> return $ VInt (xx + yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx + yy)
      _ -> throwError $ TypeError "Adding values of different types"
  x :* y -> do
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VInt (xx * yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx * yy)
      _ -> throwError $ TypeError "Multiplying values of different types"
  x :- y -> do
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VInt (xx - yy)
      (VFloat xx, VFloat yy) -> return $ VFloat (xx - yy)
      _ -> throwError $ TypeError "Subtracting values of different types"
  x :/ y -> do 
    xv <- evalExp x env 
    yv <- evalExp y env
    case (xv, yv) of 
      (VInt xx, VInt 0) -> throwError $ DivByZeroError "Dividing by 0"
      (VInt xx, VInt yy) -> return $ VInt (xx `div` yy)
      -- (VFloat xx, VFloat yy) -> return $ VFloat (xx / yy)
      _ -> throwError $ TypeError "Dividing values of different types"
  x :== y -> do 
    xv <- evalExp x env
    yv <- evalExp y env 
    case (xv, yv) of 
      (VInt xx, VInt yy) -> return $ VBool (xx == yy)   
      (VFloat xx, VFloat yy) -> return $ VBool (xx == yy)
      (VBool xx, VBool yy) -> return $ VBool (xx == yy)
      _ -> throwError $ TypeError "Checking equality on different types"     
  x :$ y -> do
    xv <- evalExp x env 
    yv <- evalExp y env 
    case xv of 
      VLam str env' exp -> evalExp exp ((str, yv) : env')
      _ -> throwError $ TypeError "Application not on a lambda"
      -- ex. (\z -> z + x + 2) :$ 7 [(x, 5)]
  Not x -> do 
    xv <- evalExp x env 
    case xv of 
      VBool b -> return $ VBool (not b)
      _ -> throwError $ TypeError "Negation not on a Bool"      
  e1 :!! e2 -> undefined