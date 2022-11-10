

-- Parser + interpreter példa
--------------------------------------------------------------------------------

{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Data.Char  -- isDigit, isAlpha, digitToInt
import Control.Monad.State

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

rightAssoc :: (a -> a -> a) -> Parser a            -> Parser sep -> Parser a
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

-- nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa


-- "While" parser interpreter
--------------------------------------------------------------------------------

-- "While" nyelv
-- while ciklus
-- értékadás / új változó felvétel
-- aritmetika/Bool kifejezések

{-
x := 0;
y := 1;
while (not (x == 10)) do
  x := x + 1;
end
-}

-- futás után megkapjuk az összes változó végső értékét

type Program = [Statement]  -- ;-vel elválasztott Statement-ek

data Exp
  = Var String     -- nemüres betűsorozat
  | BoolLit Bool   -- true|false
  | IntLit Int     -- pozitív számliterál
  | Not Exp        -- not e
  | Add Exp Exp    -- e1 + e2
  | And Exp Exp    -- e1 && e2
  | Or Exp Exp     -- e1 || e2
  | Eq Exp Exp     -- e1 == e2
  deriving (Eq, Show)

data Statement
  = Assign String Exp   -- x := e
  | While Exp Program   -- while e1 do prog end
  deriving (Eq, Show)


{- kifejezés precendenciák:
   - atom: literálok, változónevek (nemüres betűsorozat), zárójelezett kifejezés
   - not:  prefix művelet
   - +:   jobb asszoc
   - &&:  jobb asszoc
   - ||:  jobb asszoc
   - ==:  nem asszociatív
-}

-- Statement-nél nem kell precedenciával foglalkozni (nincs operátor, minden egyértelműen olvasható)
-- whitespace: isSpace függvénnynek megfelelően

-- Parser
------------------------------------------------------------

ws :: Parser ()
ws = many_ (satisfy isSpace)

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]   -- azokat az elemeket soroljuk fel, amelyek átfednek az azonosítókkal
keywords = ["while", "do", "end", "not", "true", "false"]

-- "gyengén típusozott" parser, előfeltétel: s benne van keywords-ben
pKeyword' :: String -> Parser ()
pKeyword' s = do
  string s
  (satisfy isLetter >> empty) <|> ws

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

-- kifejezés parser
------------------------------------------------------------

pPos' :: Parser Int
pPos' = pPos <* ws

pAtom :: Parser Exp
pAtom =  (Var <$> pIdent')
     <|> (BoolLit True  <$ pKeyword' "true")
     <|> (BoolLit False <$ pKeyword' "false")
     <|> (IntLit <$> pPos')
     <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot = prefix Not pAtom (pKeyword' "not")

pAdd :: Parser Exp
pAdd = rightAssoc Add pNot (char' '+')

pAnd :: Parser Exp
pAnd = rightAssoc And pAdd (string' "&&")

pOr :: Parser Exp
pOr = rightAssoc Or pAnd (string' "||")

pEq :: Parser Exp
pEq = nonAssoc Eq pOr (string' "==")

pExp :: Parser Exp -- tetszőleges Exp parser függvény
pExp = pEq

-- Statement
--------------------------------------------------------------------------------

pStatement :: Parser Statement
pStatement =
      (Assign <$> pIdent' <*> (string' ":=" *> pExp))
  <|> (While <$> (pKeyword' "while" *> pExp)
             <*> (pKeyword' "do" *> pProgram <* pKeyword' "end"))

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pTop :: Parser Program
pTop = ws *> pProgram <* eof

parseProgram :: String -> Maybe Program
parseProgram s = fst <$> runParser pTop s

p1 = unlines [
  "x := 1;",
  "y := 10;",
  "z := x + y + 20"
  ]

-- Interpreter
--------------------------------------------------------------------------------

-- Exp kiértékelés: értelemszerűen
--                  Eq működik Int-re és Bool-ra (azonos típusú argumentumok)

-- Értékadás:
--    - változókörnyezet: névhez Int vagy Bool értéket rendel
--    - értékadás: - ha új változó, akkor felvesszük az új változót a környezetbe
--                 - egyébként módosítjuk a meglévő értéket
--                 - while testben felvett új változó nem látható kívülről

data Val = VInt Int | VBool Bool
  deriving (Eq, Show)

type Env = [(String, Val)]

typeError :: a
typeError = error "type error"

evalExp :: Env -> Exp -> Val
evalExp env = go where
  go :: Exp -> Val
  go e = case e of
    Var x -> case lookup x env of
      Nothing -> error $ "name not in scope: " ++ x
      Just v  -> v
    BoolLit b -> VBool b
    IntLit n  -> VInt n
    Not e     -> case go e of
                   VBool b -> VBool (not b)
                   _       -> typeError
    Add e1 e2 -> case (go e1, go e2) of
                   (VInt n1, VInt n2) -> VInt (n1 + n2)
                   _                  -> typeError

    And e1 e2 -> case (go e1, go e2) of
                   (VBool b1, VBool b2) -> VBool (b1 && b2)
                   _                    -> typeError

    Or e1 e2 -> case (go e1, go e2) of
                   (VBool b1, VBool b2) -> VBool (b1 || b2)
                   _                    -> typeError

    Eq e1 e2 -> case (go e1, go e2) of
                   (VBool b1, VBool b2) -> VBool (b1 == b2)
                   (VInt n1 , VInt n2)  -> VBool (n1 == n2)
                   _                    -> typeError

-- ha valami newScope-ban fut, akkor a futás után az újonnan felvett változókat
-- eldobjuk az Env-ből.
inNewScope :: State Env a -> State Env a
inNewScope ma = do
  l <- length <$> get
  a <- ma
  modify (take l)
  pure a

updateEnv :: String -> Val -> Env -> Env
updateEnv x v [] = [(x, v)]
updateEnv x v ((y, v'):env)
  | x == y    = (x, v):env
  | otherwise = (y, v'):updateEnv x v env

evalStatement :: Statement -> State Env ()
evalStatement s = case s of
  Assign x e -> do
    env <- get
    let v = evalExp env e
    put $ updateEnv x v env
  While e prog -> do
    v <- evalExp <$> get <*> pure e
    case v of
      VBool True  -> inNewScope (evalProgram prog) >> evalStatement (While e prog)
      VBool False -> pure ()
      VInt _      -> typeError

evalProgram :: Program -> State Env ()
evalProgram = mapM_ evalStatement

runProgram :: String -> Env
runProgram s = case parseProgram s of
  Nothing -> error "parse error"
  Just p  -> execState (evalProgram p) []
