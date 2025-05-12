{-# LANGUAGE LambdaCase #-}

module Vizsga where
import Control.Monad.Except
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader (ReaderT(runReaderT), ask, local)
import Control.Monad.Writer (WriterT(runWriterT), tell)
import Control.Monad.IO.Class

data NTree a = Bind a [NTree a]
  deriving (Show, Eq)

instance Functor (NTree) where
  fmap :: (a -> b2) -> NTree a -> NTree b2
  fmap = undefined

instance Foldable (NTree) where
  foldMap :: Monoid m => (a -> m) -> NTree a -> m
  foldMap f (Bind a r) = undefined

  foldr :: (a -> b -> b) -> b  -> NTree a -> m
  foldr f b (Bind a r) = undefined


instance Traversable (NTree) where
  sequenceA :: Applicative f => NTree (f a) -> f (NTree a)
  sequenceA = undefined

  traverse :: Applicative f => (a -> f b) -> NTree a -> f (NTree b)
  traverse = undefined
  

t1, t2, t3 :: NTree Integer
t1 = Bind 10 [Bind 3 [], Bind 5 [ Bind 1 [], Bind 2 []], Bind 9 [Bind 6 []]]
t2 = Bind 0 []
t3 = Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [ Bind 8 []]]]]]]]

t4 :: NTree (NTree Integer)
t4 = Bind t3 [Bind t1 [Bind t2 [], Bind t1 [], Bind t2 [Bind t1 []]]]

{-
>>> sum t1 == 36
True

>>> not $ and $ fmap (<5) t3
True

>>> fmap (+1) t1 == Bind 11 [Bind 4 [],Bind 6 [Bind 2 [],Bind 3 []],Bind 10 [Bind 7 []]]
True

>>> (3 <$ t2) == Bind 3 []
True

>>> traverse (\a -> if a > 9 then Just (a ^ 2) else Nothing) t3 == Nothing
True

>>> traverse (\a -> if a < 9 then Just (a ^ 2) else Nothing) t3 == Just (Bind 1 [Bind 4 [Bind 9 [Bind 16 [Bind 25 [Bind 36 [Bind 49 [Bind 64 []]]]]]]])
True

-}

unNest :: NTree (NTree a) -> NTree a
unNest = undefined

-- >>> unNest t4 == Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [Bind 8 []]]]]]],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]]]]]
-- True

-- >>> unNest (Bind (Bind 1 []) []) == Bind 1 []
-- True

-- >>> unNest (Bind (Bind 2 [Bind 3 []]) []) == Bind 2 [Bind 3 []]
-- True

-- >>> unNest (Bind (Bind 3 []) [Bind (Bind 2 []) []]) == Bind 3 [Bind 2 []]
-- True

-- >>> unNest (Bind (Bind 3 []) [Bind (Bind 2 [Bind 5 []]) [] , Bind (Bind 7 []) []]) == Bind 3 [Bind 2 [Bind 5 []],Bind 7 []]
-- True

-- >>> unNest (Bind (Bind 4 [Bind 1 []]) [Bind (Bind 5 []) []]) == Bind 4 [Bind 1 [],Bind 5 []]
-- Bind 4 [Bind 1 [],Bind 5 []]

flattenT :: NTree a -> [a]
flattenT = undefined
-- >>> flattenT t1 == [10,3,5,1,2,9,6]
-- True
-- >>> flattenT t2 == [0]
-- True
-- >>> flattenT t3 == [1,2,3,4,5,6,7,8]
-- True
-- >>> flattenT t4 == [Bind 1 [Bind 2 [Bind 3 [Bind 4 [Bind 5 [Bind 6 [Bind 7 [Bind 8 []]]]]]]],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]],Bind 0 [],Bind 10 [Bind 3 [],Bind 5 [Bind 1 [],Bind 2 []],Bind 9 [Bind 6 []]]]
-- True

notAllButHead :: (a -> Bool) -> NTree a -> Bool
notAllButHead f = undefined
-- >>> not (notAllButHead (== 3) t2)
-- True
-- >>> notAllButHead (<= 1) t3
-- True

--- Trafó

-- Modellezzünk egy HTTP szervert Monád Trafók segtségével!
-- WriterT [String]: Log-olási környezet
-- StateT [(Int, Int, Maybe String)]: Üzenet azonosító, HTTP kód, és üzenet szövege
-- ReaderT [(String, String)]: Elérhető fájlok
-- IO: A legbelső monád

runServer = runWriterT . flip runStateT ([] :: [(Int, Int, Maybe String)]) . flip runReaderT []

repeateTimes :: (Ord t, Num t, Monad f) => t -> f a -> f ()
repeateTimes i | i <= 0 = const $ pure ()
repeateTimes i = \b -> b >> repeateTimes (i - 1) b

-- type Server a = undefined

-- Írjuk meg a "send" függvényt, ami egy paraméterül kapott fájl elérési útjából megzserzi a fájlt, logolja, hogy létezik-e, és amnnyiben létezik,
-- írjuk bele a state-be az eddigi legnagyobb azonosító +1-es azonosítóval, és a "megfelelő" HTTP kóddal.
-- Megtaláltulk a fájlt: 200-as a kód, és a fájl szövege a String
-- Nem találtuk meg a fájlt: 404-es hibakód, és a szöveg "The file cannot be found"

send :: undefined
send = undefined
-- >>> (== (((),[(1,404,Nothing)]),["The file was not found!"])) <$> (runServer (send "test.txt"))
-- True

-- Készítsük el az "append" függvényt, ami kap paraméterül egy elérési utat, és hozzá egy fájl tartalmat.
-- Visszatérési értéke egy lista, ami tartalmazza a "megváltoztatott" olvasási környezetet.
-- Amennyiben az olvasási környezet tartalmazta az eddigi fájl elérési útját, ezt log-oljuk, és "írjuk felül"!
-- Amennyiben nem tartalmazta, adjuk hozzá, és ezt is log-oljuk!

append :: (String, String) -> Server [(String, String)]
append = undefined

--- >>> (== (([("test.txt","Én elmentem a vásárba")],[]),["Updating file tree with: test.txt"])) <$> (runServer (append ("test.txt", "Én elmentem a vásárba")))
-- True

-- >>> (== (((),[(1,200,Just "Én elmentem a vásárba")]),["Updating file tree with: test.txt","Path: test.txt  was found!","Value: Én elmentem a vásárba"])) <$> (runServer (append ("test.txt", "Én elmentem a vásárba") >>= \n -> local (const n) (send "test.txt")))
-- True
--

-- Készítsük el a "start" függvényt, ami "elindítja" a szerverünket. A függvény olvasson be a konzolról egy sort (getLine).
-- Amennyiben a sor ':'-val kezdődik, hívjuk meg a ':' karatker utáni elérési úttal az "append" függvényt, aminek a paraméterei
-- az előbb elmített elérési út, majd utána egy szóközzel elválasztva a maradéka a beolvasott szövegnek. (takeWhile!)
-- Ezek után hívjuk meg rekurzívan az új függvényt az új írási környezettel.
-- Pl: ":test.txt Én elmentem a vásárba." -> append "test.txt" "Én elmentem a vásárba."
-- Amennyiben nem kezdődik ':'-tal, úgy kezeljük a beolvasott sort, mintha elérési út lenne. Erre az elérési útra hívjuk meg a "send" függvényt!
-- Miután végeztünk a függvény hívásokkal, írjuk ki a log-olási környezetben található String-eket a konzolra!


start :: undefined
start = undefined

--- Nyelv kiegészítés

-- Parser

-- Egészítsük ki a nyelvet lokális scope-okkal.
-- - Ehhez vegyünk fel 2 kulcsszót: `do` és `with`
-- - Adjuk hozzá a szintaxishoz a `Do :: [Statement] -> Maybe [Statement] -> Statement` állatást!
-- - A `Do` szintaxis a `do` kulcsszóval kezdődjön, és az `end` kulcsszóval végződjön. A két kulscszó között egy programot várunk.
-- - Opcionálisan cserélhessük le az `end` -et egy `with`-re, amely után kifejezéseket várunk. Az a blokk is záruljon `end` kulcsszóval!

-- Interpreter
-- - Amennyiben nincsen `with` kiefejezésünk, a `do` blokkban lévő programot futtassuk le!
-- - A `do` blokkban történő környezeti változások (új változó felvétele, stb) ne hasson ki a do blokkkon kívülre!
-- - Amennyiben van `with` blokkunk, annak tagjait vegyük hozzá a `do` blokk környezetéhez!
-- - Amennyiben a `with`-ben nem csak értékadások vannak, dobjunk egy új hibát: `InvaidExpressionInWith:: String -> InterpreterError`

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
keywords = ["true", "false", "not", "while", "if", "then", "end"]

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
  deriving (Show, Eq)

-- Írjunk ezekre parsereket!
-- Egy programkód egyes sorait ;-vel választjuk el

program :: Parser [Statement]
program = sepBy statement (char' ';')

statement :: Parser Statement
statement = asum [
      sIf,
      sWhile,
      sAssign,
      sDo
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
sWhile = pKeyword "while" *> (While <$> (pExp <* pKeyword "do") <*> (program <* pKeyword "end"))


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
>>> runProgram "y := 1; x := (lam k -> k); while not (y == 2) do x := (lam k -> x $ k * k); y := y + 1;  end; z := x $ 2"
Right ((),[("z",VInt 4),("y",VInt 2),("x",VLam "k" [("x",VLam "k" [("y",VInt 1)] (Var "k")),("y",VInt 1)] (Var "x" :$ (Var "k" :* Var "k")))])
-}

-- >>> runProgram "y := 1; x := (lam k -> k); while not (y == 2) do x := (lam k -> x $ k * k); y := y + 1;  end; z := x $ 2"
-- Right ((),[("z",VInt 4),("y",VInt 2),("x",VLam "k" [("x",VLam "k" [("y",VInt 1)] (Var "k")),("y",VInt 1)] (Var "x" :$ (Var "k" :* Var "k")))])
--


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
  deriving (Show, Eq)

type Env = [(String, Val)] -- a jelenlegi környezet

data InterpreterError
  = TypeError String -- típushiba üzenettel
  | ScopeError String -- variable not in scope üzenettel
  | DivByZeroError String -- 0-val való osztás hibaüzenettel
  | CompiledError String
  | InvaidExpressionInWith String
   deriving (Show, Eq)

-- Az interpreter típusát nem adjuk meg explicit, hanem használjuk a monád transzformerek megkötéseit!
-- Értékeljünk ki egy kifejezést!
evalExp :: MonadError InterpreterError m => Exp -> Env -> m Val
evalExp (IntLit i) _ = pure $ VInt i           -- 1 2 ...
evalExp (FloatLit d) _ = pure $ VFloat d
evalExp (BoolLit b ) _ = pure $ VBool b
evalExp (Var s  ) env = case lookup s env of
  Nothing -> throwError $ ScopeError $ "Unknown variable called: " ++ s
  (Just x) -> pure x
evalExp ( LamLit s exp ) env = pure $ VLam s env exp
evalExp e@(l :+ r) env = do
  val <- (,) <$> evalExp l env <*> evalExp r env
  case val of
    (VInt i, VInt j) -> pure $ VInt (i + j)
    (VFloat f, VFloat g) -> pure $ VFloat (f + g)
    (VInt i, VFloat g) -> pure $ VFloat (fromIntegral i + g)
    (VFloat f, VInt j) -> pure $ VFloat (f + fromIntegral j )
    _ -> throwError $ TypeError $ "Bad type in addition expression: " ++ show e
evalExp e@(l :* r) env = do
  val <- (,) <$> evalExp l env <*> evalExp r env
  case val of
    (VInt i, VInt j) -> pure $ VInt (i * j)
    (VFloat f, VFloat g) -> pure $ VFloat (f * g)
    (VInt i, VFloat g) -> pure $ VFloat (fromIntegral i * g)
    (VFloat f, VInt j) -> pure $ VFloat (f * fromIntegral j )
    _ -> throwError $ TypeError $ "Bad type in multiplication expression: " ++ show e
evalExp e@(l :- r) env = do
  val <- (,) <$> evalExp l env <*> evalExp r env
  case val of
    (VInt i, VInt j) -> pure $ VInt (i - j)
    (VFloat f, VFloat g) -> pure $ VFloat (f - g)
    (VInt i, VFloat g) -> pure $ VFloat (fromIntegral i - g)
    (VFloat f, VInt j) -> pure $ VFloat (f - fromIntegral j )
    _ -> throwError $ TypeError $ "Bad type in substraction expression: " ++ show e
evalExp e@(l :/ r) env = do
  val <- (,) <$> evalExp l env <*> evalExp r env
  case val of
    (_, VInt 0)-> throwError $ DivByZeroError $ "Tried to divide by zero in expression: " ++ show e
    (_, VFloat f) | abs f <= 0.0000000001 -> throwError $ DivByZeroError $ "Tried to divide by zero in expression: " ++ show e
    (VInt i, VInt j) -> pure $ VInt (i `div` j)
    (VFloat f, VFloat g) -> pure $ VFloat (f / g)
    (VInt i, VFloat g) -> pure $ VFloat (fromIntegral i / g)
    (VFloat f, VInt j) -> pure $ VFloat (f / fromIntegral j )
    _ -> throwError $ TypeError $ "Bad type in division expression: " ++ show e
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
evalExp k@(l :&& r ) env = do
  lm <- evalExp l env
  rm <- evalExp r env
  case (lm, rm) of
    ((VBool lv), (VBool rv)) -> pure $ VBool (lv && rv)
    _ -> throwError $ TypeError $ "Invalid type in expression (the expect type was bool) in: " ++ show k

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
      when boo $ do
        evalProgram b
        evalStatement w
    _ -> throwError $ TypeError $ "The expression: (" ++ show exp ++ ") is not a bool in the While statement!"
evalStatement (Do st Nothing) = undefined
evalStatement w@(Do st (Just v)) = traverse


evalProgram :: (MonadError InterpreterError m, MonadState Env m) => [Statement] -> m ()
evalProgram = mapM_ evalStatement

runProgram :: String -> Either InterpreterError ((), Env)
runProgram p = case parseProgram p of
  Right xs -> runStateT ((evalProgram xs) :: StateT Env (Either InterpreterError) ()) [];
  Left xs -> Left $ CompiledError xs

-- >>> runParser program "x := 1; do x := 2 with x:= 3 end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 2)] (Just [Assign "x" (IntLit 3)])],"")
-- True

-- >>> runParser program "x := 1; do x := 2; y := 4 with x:= 3; end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 2),Assign "y" (IntLit 4)] (Just [Assign "x" (IntLit 3)])],"")
-- True

-- >>> runParser program "x := 1; do x := 1 / x; y := 4 with x := 0; end" == Right ([Assign "x" (IntLit 1),Do [Assign "x" (IntLit 1 :/ Var "x"),Assign "y" (IntLit 4)] (Just [Assign "x" (IntLit 0)])],"")
-- True

-- >>> runParser program "x := 1; do with while x == 1 do end end" == Right ([Assign "x" (IntLit 1),Do [] (Just [While (Var "x" :== IntLit 1) []])],"")
-- True

-- >>> runProgram "x := 1; do x := 2; y := 4 with x:= 3; end" == Right ((),[("x",VInt 1)])
-- True

-- >>> runProgram "x := 1; do x := 1 / x; y := 4 with x := 0; end" == Left (DivByZeroError "Tried to divide by zero in expression: IntLit 1 :/ Var \"x\"")
-- True

-- >>> runProgram "x := 1; do with while x == 1 do end end" == Left (InvaidExpressionInWith "Illegal statement in expression: [While (Var \"x\" :== IntLit 1) []]")
-- True

-- Egészítsük ki a nyelvet egy print állítással (hint: MonadIO megkötés)
-- Egészítsük ki a nyelvet más típusokkal (tuple, either stb)

