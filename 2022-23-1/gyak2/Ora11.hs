{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs #-}
module Ora11 where

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


chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
    val <- v
    (do
        opr <- op
        res <- chainr1 v op
        pure (opr val res)
        ) <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
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

postfixAssoc :: Parser a -> Parser (a -> a) -> Parser a
postfixAssoc pa op = (pa >>= parseLeft) <|> pa
  where
    parseLeft a = ((($ a) <$> op) >>= parseLeft) <|> pure a

postfix :: Parser a -> Parser (a -> a) -> Parser a
postfix pa op = (flip ($) <$> pa <*> op) <|> pa

prefixAssoc :: Parser a -> Parser (a -> a) -> Parser a
prefixAssoc pa op = (op <*> prefixAssoc pa op) <|> pa

prefix :: Parser a -> Parser (a -> a) -> Parser a
prefix pa op = (op <*> pa) <|> pa

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
    | Not Exp
    | Fact Exp
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Pow Exp Exp
    deriving (Eq, Show)

reservedKeywords :: [String]
reservedKeywords = ["if", "then", "else", "while", "do", "end", "true", "false", "for", "testing", "in", "not"]

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

pNot :: Parser Exp
pNot = prefix pAtom (Not <$ tok (keyword' "not"))

pFact :: Parser Exp
pFact = postfixAssoc pNot (Fact <$ tok (char '!'))

pPow :: Parser Exp
pPow = chainr1 pFact (Pow <$ tok (char '^'))

pMul :: Parser Exp
pMul = chainl1 pPow (Mul <$ tok (char '*'))

pAdd :: Parser Exp
pAdd = chainl1 pMul (Add <$ tok (char '+') <|> Sub <$ tok (char '-'))

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
    | For Statement Exp Statement [Statement]
    deriving (Eq, Show)

program :: Parser [Statement]
program = sepBy1 (pAssign <|> pAssignAdd <|> pAssignSub <|> pITE <|> pWhile <|> pFor) (tok $ char ';')

pAssign :: Parser Statement
pAssign = Assign <$> nonKeyword <*> (tok (string ":=") *> pExp)

-- a += 5
-- a := a + 5
pAssignAdd :: Parser Statement
pAssignAdd = do
    vName <- nonKeyword
    tok (string "+=")
    exp <- pExp
    pure $ Assign vName (Add (Var vName) exp)

pAssignSub :: Parser Statement
pAssignSub = do
    vName <- nonKeyword
    tok (string "-=")
    exp <- pExp
    pure $ Assign vName (Sub (Var vName) exp)

pITE :: Parser Statement
pITE = IfThenElse <$> (keyword' "if" *> pExp <* keyword' "then") <*> (program <* keyword' "else") <*> (program <* keyword' "end")

pWhile :: Parser Statement
pWhile = While <$> (keyword' "while" *> pExp <* keyword' "do") <*> (program <* keyword' "end")

pFor :: Parser Statement
pFor = For <$> (keyword' "for" *> pAssign <* keyword' "testing") 
           <*> (pExp <* keyword' "in")
           <*> ((pAssign <|> pAssignAdd <|> pAssignSub) <* keyword' "do")
           <*> (program <* keyword' "end")

-- KIÉRTÉKELÉS

data Value = VInt Int | VBool Bool deriving Show
type Env = [(String, Value)]

evalExp :: Env -> Exp -> Value
evalExp env exp = case exp of
    Var str -> case lookup str env of
        Just v -> v
        Nothing -> error $ "Variable not in scope: " ++ str
    IntLit int -> VInt int
    BoolLit bool -> VBool bool
    Not exp -> case evalExp env exp of
        VBool a -> VBool $ not a
        _       -> error "Type is not a bool."
    Fact exp -> case evalExp env exp of
        VInt n | n >= 0 -> VInt $ product [1..n]
        VInt _          -> error "Factorial of negative doesn't exist!"
        _               -> error "Type is not a number."
    Eq exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VBool (int1 == int2)
        (VBool b1, VBool b2)   -> VBool (b1 == b2)
        _                      -> error "Types do not match in =="
    Leq exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VBool (int1 <= int2)
        _                      -> error "Type is not correct"
    Lt exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VBool (int1 < int2)
        _                      -> error "Type is not correct"
    Add exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VInt (int1 + int2)
        _                      -> error "Type is not a number"
    Sub exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VInt (int1 - int2)
        _                      -> error "Type is not a number"
    Mul exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VInt (int1 * int2)
        _                      -> error "Type is not a number"
    Pow exp1 exp2 -> case (evalExp env exp1, evalExp env exp2) of
        (VInt int1, VInt int2) -> VInt (int1 ^ int2)
        _                      -> error "Type is not a number"  

evalStatement :: Statement -> State Env ()
evalStatement (Assign str exp) = do
    env <- get
    put $ modify' str env (evalExp env exp)
evalStatement (IfThenElse exp prog1 prog2) = do
    env <- get
    case evalExp env exp of
        VInt _ -> error "If has bad type"
        VBool True -> runInScope $ void $ traverse evalStatement prog1
        VBool False -> runInScope $ void $ traverse evalStatement prog2
evalStatement (While exp prog) = do
    env <- get
    case evalExp env exp of
        VInt _ -> error "ohno"
        VBool True -> do
            runInScope $ void $ traverse evalStatement prog
            evalStatement (While exp prog)
        VBool False -> pure ()
evalStatement (For q cond r prog) =
    runInScope $ do
        evalStatement q
        env <- get
        case evalExp env cond of
            VInt _ -> error "Condition must be Bool!"
            VBool True -> 
                evalFor cond r prog where 
                    evalFor cond r prog = do
                        runInScope $ void $ traverse evalStatement prog
                        evalStatement r
                        env' <- get
                        case evalExp env' cond of
                            VInt _ -> error "Condition must be Bool!"
                            VBool True -> evalFor cond r prog
                            VBool False -> pure ()
            VBool False -> pure ()
{-
for (int i = 0; i < 10; i++)
     ^^^^^^^^^
    ez csak egyszer fut le
{
   ... 
}
-}

modify' :: String -> Env -> Value -> Env
modify' str [] v = [(str, v)]
modify' str ((x,y):xs) v
    | str == x = (x,v) : xs
    | otherwise = (x,y) : modify' str xs v

evalProgram :: [Statement] -> Env
evalProgram ss = fst $ runState (mapM_ evalStatement ss) []

runInScope :: State Env () -> State Env ()
runInScope st = do
    env <- get
    st
    env' <- get
    put $ compare' env env'

compare' :: Env -> Env -> Env
compare' [] _ = []
compare' ((s,v):xs) ys = case lookup s ys of
    Just v' -> (s,v') : compare' xs ys
    Nothing -> error "hiba" 

-- Parser és interpreter összekötése
-- A parser ugye nem feltétlenül sikeres, ezért nem biztos, hogy kapunk vissza eredményt
-- Lefuttatjuk a parsert -> mintaillesztés -> ha tudjuk, lefuttatjuk az interpretert
runProgram :: String -> Maybe Env
runProgram str = case runParser program str of
    Just ("",a) -> Just $ evalProgram a
    _           -> Nothing

-- Fájlból olvasásos verzió
-- Használjuk a readFile :: FilePath -> IO String függvényt
-- Írjuk ki a terminálra az env-et

-- type FilePath = String
runFromFile :: FilePath {- Lényegében csak egy string -} -> IO ()
runFromFile path = do
    content <- readFile path
    print $ runProgram content

{-

    Nyelvhez egyéb funkciók csatolása:

    For ciklus
    - For ciklus egy állítás aminek paraméterei
        - Kezdeti állítás (i := 0)
        - Ciklus feltétel kifejezés (i < 10)
        - Ciklusiteráció állítás (i := i + 1)
        - Maga a belső program
    - Szintaxis: for <statement> testing <expression> in <statement> do <program> end

    +=, -=, *=, /= kifejezések
    pl. a += 5 -> a := a + 5
    - Szükséges a -, / kifejezések nyelvhez adása
    - Ezek mind állításokra fordulnak ezért új állítást nem kell bevezetni - szintaktikus cukorka
    - For ciklust is meg lehet szintaktikus cukorkaként írni

    Prefix és Postfix függvények (not függvény és faktoriális operátor)
    - not kulccsszó és ! operátor
    - az atom és a legnagyobb precedencia közé mennek

---------------------------

    ⊥ érték (opcionális)
    - Kiértékelhetetlen érték
    - akármilyen kifejezésre illeszkedik
    - Haskellben az undefined
    - lusta kiértékeléssel kiküszöbölhető

    Lusta kifejezés
    - Val típusnak új konstruktor: Lazy ami Exp-et vár
    - A lényeg annyi, hogy a változókat be kell helyettesíteni, hogy ne delayed Out of Scope Variable hiba legyen


-}
