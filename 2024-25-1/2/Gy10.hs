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
  | Exp :&& Exp          -- e1 && e2
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
| &&                 | Jobbra             | 9                  |
+--------------------+--------------------+--------------------+
| $                  | Jobbra             | 8                  |
+--------------------+--------------------+--------------------+

-}

keywords :: [String]
keywords = ["true", "false", "not", "while", "if", "then", "do", "end"]

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

pAnd :: Parser Exp
pAnd = chainr1 pEq ((:&&) <$ string "&&" )

pDollar :: Parser Exp
pDollar = chainr1 pAnd ((:$) <$ char' '$')

pExp :: Parser Exp -- táblázat legalja
pExp = pDollar

-- Állítások: értékadás, elágazások, ciklusok
data Statement
  = If Exp [Statement]        -- if e then p end
  | While Exp [Statement]     -- while e do p end
  | Assign String Exp         -- v := e
  deriving (Show)

-- Írjunk ezekre parsereket!
-- Egy programkód egyes sorait ;-vel választjuk el

program :: Parser [Statement]
program = sepBy statement (char' ';')

statement :: Parser Statement
statement = asum [
      sIf,
      sWhile,
      sAssign
    ] <|> throwError "statement: unable to parse any valid statements!"

sIf :: Parser Statement
sIf = do
  pKeyword "if"
  e <- pExp
  pKeyword "then"
  p <- program
  pKeyword "end"
  return (If e p)

sWhile :: Parser Statement
sWhile =  do
  pKeyword "while"
  e <- pExp
  pKeyword "do"
  p <- program
  pKeyword "end"
  return (While e p)

{-
pKeyword "while" 
>>= _ -> pExp 
>>= \e -> pKeyword "do" 
>>= \_ -> p <- program
>>= \p -> pKeyword "end"
>>= \_ -> pure (While e p)

-}

{-
pKeyword "While" *> (While <$> (pExp <* pKeyword "do") <*> (program <* pKeyword "end"))
-}

sAssign :: Parser Statement
sAssign = Assign <$> pNonKeyword <*> (string' ":=" *> pExp)

{-
>>> runProgram "y := 1; x := (lam k -> k); while not (y == 3) do x := (lam k -> x $ k * k); y := y + 1;  end; z := x $ 2"
Right ((),[("z",VInt 16),("y",VInt 3),("x",VLam "k" [("y",VInt 2),("x",VLam "k" [("x",VLam "k" [("y",VInt 1)] (Var "k")),("y",VInt 1)] (Var "x" :$ (Var "k" :* Var "k")))] (Var "x" :$ (Var "k" :* Var "k")))])
-}


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

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError String -- típushiba üzenettel
  | ScopeError String -- variable not in scope üzenettel
  | DivByZeroError String -- 0-val való osztás hibaüzenettel
  | CompiledError String
   deriving (Show)

-- Az interpreter típusát nem adjuk meg explicit, hanem használjuk a monád transzformerek megkötéseit!
-- Értékeljünk ki egy kifejezést!
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp (IntLit i) env = pure $ VInt i           -- 1 2 ...
evalExp (FloatLit d) env = pure $ VFloat d
evalExp (BoolLit b ) env = pure $ VBool b
evalExp (Var s  ) env = case lookup s env of
  Just v  -> pure v
  Nothing -> throwError $ ScopeError s
evalExp ( LamLit s exp ) env = pure $ VLam s env exp
evalExp ( (IntLit i) :+ (IntLit j)  ) env = pure $ VInt (i + j)
evalExp ( (FloatLit i) :+ (FloatLit j)  ) env = pure $ VFloat (i + j)
evalExp ( (FloatLit f) :+ (IntLit j)  ) env = pure $ VFloat (f + fromIntegral j)
evalExp ( (IntLit f) :+ (FloatLit j)  ) env = pure $ VFloat (fromIntegral f + j)
evalExp k@( (Var s) :+ r  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp ((IntLit i) :+ r) env
  (Just (VFloat f)) -> evalExp ((FloatLit f) :+ r) env
  _ -> throwError $ TypeError  $ "Left handside of addition is not a valid type in exp: " ++ show k
evalExp k@( l :+ (Var s)  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp (l :+ (IntLit i)) env
  (Just (VFloat f)) -> evalExp (l :+ (FloatLit f)) env
  _ -> throwError $ TypeError  $ "Right handside of addition is not a valid type in exp: " ++ show k
evalExp k@( _ :+ _  ) env = throwError $ TypeError  $ "Invalid tpe in expression: " ++ show k
evalExp ( (IntLit i) :* (IntLit j)  ) env = pure $ VInt (i * j)
evalExp ( (FloatLit i) :* (FloatLit j)  ) env = pure $ VFloat (i * j)
evalExp ( (FloatLit f) :* (IntLit j)  ) env = pure $ VFloat (f * fromIntegral j)
evalExp ( (IntLit f) :* (FloatLit j)  ) env = pure $ VFloat (fromIntegral f * j)
evalExp k@( (Var s) :* r  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp ((IntLit i) :* r) env
  (Just (VFloat f)) -> evalExp ((FloatLit f) :* r) env
  _ -> throwError $ TypeError  $ "Left handside of multiplication is not a valid type in exp: " ++ show k
evalExp k@( l :* (Var s)  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp (l :* (IntLit i)) env
  (Just (VFloat f)) -> evalExp (l :* (FloatLit f)) env
  _ -> throwError $ TypeError  $ "Right handside of multiplication is not a valid type in exp: " ++ show k
evalExp k@( _ :* _  ) env = throwError $ TypeError  $ "Invalid type in expression: " ++ show k

evalExp ( (IntLit i) :- (IntLit j)  ) env = pure $ VInt (i - j)
evalExp ( (FloatLit i) :- (FloatLit j)  ) env = pure $ VFloat (i - j)
evalExp ( (FloatLit f) :- (IntLit j)  ) env = pure $ VFloat (f - fromIntegral j)
evalExp ( (IntLit f) :- (FloatLit j)  ) env = pure $ VFloat (fromIntegral f - j)
evalExp k@( (Var s) :- r  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp ((IntLit i) :- r) env
  (Just (VFloat f)) -> evalExp ((FloatLit f) :- r) env
  _ -> throwError $ TypeError  $ "Left handside of substraction is not a valid type in exp: " ++ show k
evalExp k@( l :- (Var s)  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp (l :- (IntLit i)) env
  (Just (VFloat f)) -> evalExp (l :- (FloatLit f)) env
  _ -> throwError $ TypeError  $ "Right handside of substraction is not a valid type in exp: " ++ show k
evalExp k@( _ :- _  ) env = throwError $ TypeError  $ "Invalid type in expression: " ++ show k
evalExp ( (IntLit i) :/ (IntLit j)  ) env = pure $ VInt (i `div` j)
evalExp ( (FloatLit i) :/ (FloatLit j)  ) env = pure $ VFloat (i / j)
evalExp ( (FloatLit f) :/ (IntLit j)  ) env = pure $ VFloat (f / fromIntegral j)
evalExp ( (IntLit f) :/ (FloatLit j)  ) env = pure $ VFloat (fromIntegral f / j)
evalExp k@( (Var s) :/ r  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp ((IntLit i) :/ r) env
  (Just (VFloat f)) -> evalExp ((FloatLit f) :/ r) env
  _ -> throwError $ TypeError  $ "Left handside of division is not a valid type in exp: " ++ show k
evalExp k@( l :/ (Var s)  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VInt i)) -> evalExp (l :/ (IntLit i)) env
  (Just (VFloat f)) -> evalExp (l :/ (FloatLit f)) env
  _ -> throwError $ TypeError  $ "Right handside of division is not a valid type in exp: " ++ show k
evalExp k@( _ :/ _  ) env = throwError $ TypeError  $ "Invalid type in expression: " ++ show k
evalExp k@( a :== b) env = do
  va <- evalExp a env
  vb <- evalExp b env
  let eq a b = pure $ VBool $ a == b
  case (va, vb) of
    (VBool a, VBool b) -> eq a b
    (VFloat a, VFloat b) -> eq a b
    (VInt a, VInt b) -> eq a b
    _ ->  throwError $ TypeError  $ "Invalid type in expression (The types are not the same): " ++ show k

evalExp ( (LamLit a b) :$ r  ) env = evalExp r env >>= \r -> evalExp b ((a , r) : env) 
evalExp k@(Var s :$ r)           env = case lookup s env of
  Nothing -> throwError $ ScopeError s
  (Just (VLam s nenv exp)) -> evalExp r env >>= \r -> evalExp exp ((s, r) : nenv)
  _ -> throwError $ TypeError $ "Variable is not a lambad in expression: " ++ show k
evalExp k@( _ :$ _  ) env = throwError $ TypeError $ "On the left hand side of function application there must be a lambda!\nIn expression: " ++ show k
evalExp k@( Not exp ) env = do
  m <- evalExp exp env
  case m of
    (VBool i) -> pure $ VBool $ not i
    _ -> throwError $ TypeError  $ "Invalid type in expression (The type is not bool): " ++ show k

-- Állítás kiértékelésénér egy state-be eltároljuk a jelenlegi környezetet
evalStatement :: (MonadError InterpreterError m, MonadState Env m) => Statement -> m ()
evalStatement (Assign s exp) = do
  st <- get
  case lookup s st of
    Nothing -> do
      val <- evalExp exp st
      put ((s, val) : st)
    (Just (VInt _)) -> do
      v <- evalExp exp st
      case v of
        i@(VInt _) -> put ((s, i) : filter (\(h, _) -> h /= s) st)
        _ -> throwError $ TypeError "The assigned value's type does not match!"
    (Just (VFloat _)) -> do
      v <- evalExp exp st
      case v of
        i@(VFloat _) -> put ((s, i) : filter (\(h, _) -> h /= s) st)
        _ -> throwError $ TypeError "The assigned value's type does not match!"
    (Just (VBool _)) -> do
      v <- evalExp exp st
      case v of
        i@(VBool _) -> put ((s, i) : filter (\(h, _) -> h /= s) st)
        _ -> throwError $ TypeError "The assigned value's type does not match!"
    (Just (VLam _ _ _)) -> do
      v <- evalExp exp st
      case v of
        i@(VLam _ _ _) -> put ((s, i) : filter (\(h, _) -> h /= s) st)
        _ -> throwError $ TypeError "The assigned value's type does not match!"
evalStatement (If exp b) = do
  st <- get
  val <- evalExp exp st
  case val of
    (VBool boo) -> if boo then evalProgram b else pure ()
    _ -> throwError $ TypeError $ "The expression: (" ++ show exp ++ ") is not a bool in the If statement!"
evalStatement w@(While exp b) = do
  st <- get
  val <- evalExp exp st
  case val of
    (VBool boo) -> do
      if boo then do
        evalProgram b
        evalStatement w
      else pure ()
    _ -> throwError $ TypeError $ "The expression: (" ++ show exp ++ ") is not a bool in the While statement!"

evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

runProgram :: String -> Either InterpreterError ((), Env)
runProgram p = case parseProgram p of
  Right xs -> runStateT ((evalProgram xs) :: StateT Env (Either InterpreterError) ()) [];
  Left xs -> Left $ CompiledError xs

-- Egészítsük ki a nyelvet egy print állítással (hint: MonadIO megkötés)
-- Egészítsük ki a nyelvet más típusokkal (tuple, either stb)
