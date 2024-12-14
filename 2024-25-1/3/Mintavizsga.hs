{-# LANGUAGE LambdaCase, FlexibleContexts, ExplicitForAll, StandaloneDeriving, QuantifiedConstraints#-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module Mintavizsga where

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
import Data.Either (isLeft)
-- import GhcPlugins (semi)

{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

-- Mintavizsga
-- A feladatsor megoldására 2.5 óra áll rendelkezésre.
-- Külső segítség és kollaboráció nem megengedett.
-- A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít.
-- Részpontszám kapható minden feladatra.
-- A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

-- 2: 15 - 17
-- 3: 18 - 21
-- 4: 22 - 25
-- 5: 26 - 30

-- Datás feladatok (10 pont)
------------------------------------------------------------------

data Infinitree f a = Leaf a | Node (f (Infinitree f a))
-- ignore me, mágia
deriving instance (Eq a, forall a. (Eq a) => Eq (f a)) => Eq (Infinitree f a)
deriving instance (Show a, forall a. (Show a) => Show (f a)) => Show (Infinitree f a)
-- mágia over

instance (Functor f) => Functor (Infinitree f) where
  fmap :: Functor f => (a -> b) -> Infinitree f a -> Infinitree f b
  fmap f x = case x of
    (Leaf a)    -> Leaf $ f a 
    (Node finf) -> Node $ fmap (fmap f) finf

instance Foldable f => Foldable (Infinitree f) where
  foldr :: (a -> b -> b) -> b -> Infinitree f a -> b
  foldr f b x = case x of
    (Leaf a)    -> f a b
    (Node finf) -> foldr (flip (foldr f)) b finf

instance (Traversable f) => Traversable (Infinitree f) where
  sequenceA :: (Traversable f, Applicative f1) => Infinitree f (f1 a) -> f1 (Infinitree f a)
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node finf) = Node <$> traverse sequenceA finf -- sequenceA (fmap sequenceA finf)

  traverse :: (Traversable f, Applicative f1) => (a -> f1 b) -> Infinitree f a -> f1 (Infinitree f b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node finf) = Node <$> traverse (traverse f) finf
  
a :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
a = (.) . (.)

inf1 :: Infinitree [] Int
inf1 = Node [Leaf 1, Node [Leaf 2, Leaf 3], Leaf 4, Node [Leaf 5, Leaf 6, Leaf 7]]

inf3 :: Infinitree Maybe Bool
inf3 = Node Nothing

inf4 :: Infinitree [] Bool
inf4 = Node [Leaf True, inf4]

-- Írjunk a fenti típusra (algoritmikusan generálható) Functor, Foldable és Traversable instance-ot! (5 = 1 + 2 + 2 pont)

-- deriving semmilyen formában nem használható.


testFunctor :: [Bool]
testFunctor = [
    fmap (+1) inf1 == Node [Leaf 2, Node [Leaf 3, Leaf 4], Leaf 5, Node [Leaf 6, Leaf 7, Leaf 8]],
    fmap not inf3 == inf3,
    fmap (\x -> replicate x x) inf1 == Node [Leaf [1], Node [Leaf [2,2], Leaf [3,3,3]], Leaf [4,4,4,4], Node [Leaf [5,5,5,5,5], Leaf [6,6,6,6,6,6], Leaf [7,7,7,7,7,7,7]]],
    ('a' <$ inf1) == Node [Leaf 'a', Node [Leaf 'a', Leaf 'a'], Leaf 'a', Node [Leaf 'a', Leaf 'a', Leaf 'a']]
    ]

testFoldable :: [Bool]
testFoldable = [
    sum inf1 == 28,
    and inf3,
    not (or inf3),
    or inf4
    ]

testTraversable :: [Bool]
testTraversable = [
    traverse (\x -> if x >= 1 then Just (x + 1) else Nothing) inf1 == Just (fmap (+1) inf1),
    traverse (\x -> if x > 1 then Just (x + 1) else Nothing) inf1 == Nothing
    ]

-- Definiáljuk a semiFlattener függvényt, amely kilapított formában visszaad egy fát.
-- Ha bejárás közben egy ágon fel vagy le mennénk,
-- szúrjunk be egy Left-et előtte ami azt tárolja melyik irányba mentünk egy ágban! (2 pont)

data Dir = Down | Up deriving (Eq, Show, Ord, Enum)

semiFlattener :: Infinitree [] a -> [Either Dir a]
semiFlattener (Node ls) = [Left Down] ++ (join $ map semiFlattener ls) ++ [Left Up]
semiFlattener (Leaf a) = [Right a]


testSemiFlattener :: [Bool]
testSemiFlattener = [
    semiFlattener inf1 == [Left Down, Right 1, Left Down, Right 2, Right 3, Left Up, Right 4, Left Down, Right 5, Right 6, Right 7, Left Up, Left Up],
    semiFlattener (Leaf 1) == [Right 1],
    -- semiFlattener (Node []) == [Left Down, Left Up],
    take 4 (semiFlattener inf4) == [Left Down, Right True, Left Down, Right True]
    ]

-- Definiáljuk a semiUnflattener függvényt,
-- amely egy kilapított fából visszaállítja az eredetit
-- (Feltehetjük, hogy hibásan épített fa nincs,
-- tehát minden Left Down-hoz egy Left Up is tartozik utána)! (3 pont)

semiUnflattener :: [Either Dir a] -> Infinitree [] a
semiUnflattener xs = head $ evalState go xs
  where
    go :: State [Either Dir a] [Infinitree [] a]
    go = do
      xs <- get
      case xs of
        (Left Down : xs) -> do
          put xs
          res <- go
          res' <- go
          pure $ Node res : res'
        (Left Up : xs) -> do
          put xs
          pure []
        (Right a : xs) -> do
          put xs
          xs' <- go
          return (Leaf a : xs')
        _ -> pure []


testSemiUnflattener :: [Bool]
testSemiUnflattener = [
    semiUnflattener (semiFlattener inf1) == inf1,
    -- semiUnflattener [Left Down, Left Up] == Node [],
    semiUnflattener [Right 1] == Leaf 1
    ]


-- Monad trafós feladat

{-
Legyen egy [String] típusú írási környezete (ezek lesznek az elérési logok)
Legyen egy Maybe String típusú olvasási környezete (ez lesz a jelenlegi felhasználó kártyaszáma, ha van éppen felhasználó)
Legyen egy [(String, Int)] típusú állapotváltozási környezete (ez lesz a bankkártyákhoz asszociált pénznem)
Legyen egy ATMError típusú hibakezelési környezet (ez lesz a működési hibák)
Az ATMError típusnak két konstruktora legyen: NotEnoughFunds és NoUser. A NotEnoughFunds konstruktornak legyen egy String paramétere, ami a kártyaszámot jelzi. A típusra generáljuk Eq és Show instance-ot deriving segítségével!
-}

data ATMError 
  = NotEnoughFunds String
  | NoUser
  deriving (Show, Eq)

--type ATM m = (MonadWriter [String] m, MonadReader (Maybe String) m, MonadState [(String, Int)] m, MonadError ATMError m)

type ATM a = StateT [(String, Int)] (ReaderT (Maybe String) (WriterT [String] (Except ATMError))) a

-- runAtm :: (ATM m) => m a -> [(String, Int)] -> (Maybe String) -> Either ATMError ((a, [(String, Int)]), [String])
runAtm :: StateT [(String, Int)] (ReaderT (Maybe String) (WriterT [String] (Except ATMError))) a -> [(String, Int)] -> (Maybe String) -> Either ATMError ((a, [(String, Int)]), [String])
runAtm atmMonad initialState initialUser = runExcept $ runWriterT $ runReaderT (runStateT atmMonad initialState) initialUser

-- Util

updateEnv' :: [(String, Int)] -> String -> Int -> [(String, Int)]
updateEnv' [] s v = [(s,v)]
updateEnv' ((s', v'):xs) s v
  | s == s' = (s, v) : xs
  | otherwise = (s', v') : updateEnv' xs s v

loginAs :: String -> ATM a -> ATM a 
loginAs s m = do
  tell [s ++ " belépett"]
  local (const (Just s)) m

withdrawOrDeposit :: Int -> ATM Int
withdrawOrDeposit i = do
  u <- ask
  case u of
    Just u' -> do
      env <- get
      case lookup u' env of
        (Just v) -> 
          if (v + i) < 0 
          then throwError $ NotEnoughFunds u' 
          else do
            put (updateEnv' env u' (v + i)) 
            tell [u' ++ " feltöltött " ++ show i ++ "-et"]
            return (v + i)
        Nothing -> 
          if i < 0 
          then throwError $ NotEnoughFunds u'
          else do
            put (updateEnv' env u' i)
            tell [u' ++ " feltöltött " ++ show i ++ "-et"]
            return i
    Nothing -> throwError NoUser

-- Tesztek

askATM :: ATM String
askATM = do
  s <- ask
  case s of
    Just s' -> return s'
    Nothing -> return "Nothing"

test1 = runAtm (loginAs "0000" askATM) [] Nothing == Right (("0000", []), ["0000 belépett"])
test2 = runAtm (loginAs "1100" (askATM >>= \n -> read n <$ put [(n, 9999)])) [] Nothing == Right ((1100, [("1100", 9999)]), ["1100 belépett"])
--test3 = (runAtm (loginAs "1110" (throwError NoUser)) [] Nothing :: ( Either ATMError ((), [(String, Int)]), [String])) == Left NoUser
test4 = runAtm (withdrawOrDeposit 100) [] Nothing == Left NoUser
test5 = runAtm (loginAs "0000" $ withdrawOrDeposit 1000) [] Nothing == Right ((1000, [("0000", 1000)]), ["0000 belépett", "0000 feltöltött 1000-et"])
test6 = isLeft $ runAtm (loginAs "0000" $ withdrawOrDeposit (-1000)) [] Nothing
test7 = runAtm (loginAs "1000" $ withdrawOrDeposit 1000 >> withdrawOrDeposit (-1000)) [("0101", 1234)] Nothing == Right ((0, [("0101", 1234), ("1000", 0)]), ["1000 belépett", "1000 feltöltött 1000-et", "1000 feltöltött -1000-et"])

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

{-
Adjuk a szintaxishoz a `IsLocked :: Exp -> Exp` és `Fallback :: Exp -> Exp -> Exp` kifejezéseket, illetve a `ReadWriteLock :: String -> Statement`, `WriteLock :: String -> Statement` és `Unlock :: String -> Statement` állításokat! 
-}

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
  --- Feladathoz:
  | IsLocked Exp
  | Fallback Exp Exp
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
| locked             | Prefix             | 9                  | -- NEW
+--------------------+--------------------+--------------------+
| $                  | Jobbra             | 8                  |
+--------------------+--------------------+--------------------+
| ?                  | Ballra             | 5                  | -- NEW
+--------------------+--------------------+--------------------+

-}

{-
Az IsLocked egy locked nevű prefix operátor 9-es kötési erősséggel. A locked kulcsszót adjuk hozzá a kulcsszavak listájához.
A Fallback egy ? nevű balra kötő operátor 5-ös kötési erősséggel.
-}

keywords :: [String]
keywords = 
  [ "locked"
  , "rwlock"
  , "wlock"
  , "unlock"
  , "true"
  , "false", "not", "sign", "if", "then", "do", "for", "lam", "end"]

pNonKeyword :: Parser String
pNonKeyword = do
  res <- (tok $ some (satisfy isLetter))
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

pLocked :: Parser Exp
pLocked = (IsLocked <$> (pKeyword "locked" *> pLocked)) <|> pEq

pDollar :: Parser Exp
pDollar = chainr1 pLocked ((:$) <$ char' '$')

pQuestion :: Parser Exp
pQuestion = chainl1 pDollar (Fallback <$ char' '?')

pExp :: Parser Exp -- táblázat legalja
pExp = pQuestion

-- Állítások: értékadás, elágazások, ciklusok
data Statement
  = If Exp [Statement]        -- if e then p end
  | While Exp [Statement]     -- while e do p end
  | Assign String Exp         -- v := e
  -- Feladathoz :
  | ReadWriteLock String
  | WriteLock String 
  | Unlock String
  deriving (Show, Eq)

{-
A ReadWriteLock állítás kezdődjön egy rwlock kulcsszóval, majd egy változónévvel. Az rwlock kulcsszót adjuk hozzá a kulcsszavak listájához.
A WriteLock és Unlock szintaxisa megegyezik a ReadWriteLock-éval, csak wlock és unlock kulcsszavakat használva (ezeket is adjuk hozzá a kulcsszavak listájához).
-}

program :: Parser [Statement]
program = sepBy statement (char' ';')

statement :: Parser Statement
statement = asum [sIf, sWhile, sAssign, sReadWriteLock, sWriteLock, sUnlock]

sIf :: Parser Statement
sIf = If <$> (pKeyword "if" *> pExp) <*> (pKeyword "then" *> program <* pKeyword "end")

sWhile :: Parser Statement
sWhile = While <$> (pKeyword "while" *> pExp) <*> (pKeyword "do" *> program <* pKeyword "end")

sAssign :: Parser Statement
sAssign = Assign <$> pNonKeyword <*> (pKeyword ":=" *> pExp)

-- NEW

sReadWriteLock :: Parser Statement
sReadWriteLock = do
  pKeyword "rwlock"
  v <- pNonKeyword
  return $ ReadWriteLock v 

sWriteLock :: Parser Statement
sWriteLock = do
  pKeyword "wlock"
  v <- pNonKeyword
  return $ WriteLock v 

sUnlock :: Parser Statement
sUnlock = do
  pKeyword "unlock"
  v <- pNonKeyword
  return $ Unlock v 

-- applicative-osan

sUnlock' :: Parser Statement
sUnlock' = Unlock <$> (pKeyword "unlock" *> pNonKeyword)


parseProgram :: String -> Either String [Statement]
parseProgram s = case runParser (topLevel program) s of
  Left e -> Left e
  Right (x,_) -> Right x

--Tesztek, egyes tesztek rosszak voltak

testp1 = runParser pExp "locked a" == Right (IsLocked (Var "a"), "")
testp2 = runParser pExp "not (locked a)" == Right (Not (IsLocked (Var "a")), "")
testp3 = runParser pExp "locked not a" == Right (IsLocked (Not (Var "a")),"")
testp4 = runParser pExp "locked (1 + x)" == Right (IsLocked ( (IntLit 1) :+ (Var "x")), "")
testp5 = runParser pExp "a ? 1" == Right (Fallback (Var "a") (IntLit 1), "")
testp6 = runParser pExp "a ? 1 ? b" == Right (Fallback (Fallback (Var "a") (IntLit 1)) (Var "b"), "")
testp9 = runParser program "wlock x; rwlock x; unlock x" == Right ([WriteLock "x",ReadWriteLock "x",Unlock "x"], "")
testp10 = isLeft (runParser statement "wlock 1")
testp11 = asum (runParser statement <$> ["locked := 1", "unlock := 1", "rwlock := 1", "wlock := 1"]) == Left ""

{-
Egészítsük ki a nyelvet egy LockType típussal aminek három nullaparaméteres konstruktora van, ezek a Write, ReadWrite és None (a konstruktorok lehetnek tetszőleges sorrendben és lehet tetszőleges típusosztályokat implementálni rá). deriving segítségével implementáljuk Eq instance-ot erre a típusra!

Változtassuk meg az Env típusszinonímát arra, hogy [(String, (LockType, Val))]. A fordítási hibákat az updateEnv és evalExp függvényekben javítsuk ki (per pillanat a LockType paramétert elég ignorálni).
-}

data LockType 
  = Write
  | ReadWrite
  | None
  deriving (Eq, Show)


-- Interpreter
-- Kiértékelt értékek típusa:
data Val
  = VInt Int              -- int kiértékelt alakban
  | VFloat Double         -- double kiértékelt alakban
  | VBool Bool            -- bool kiértékelt alakban
  | VLam String Env Exp   -- lam kiértékelt alakban
  deriving (Show, Eq)

type Env = [(String, (LockType, Val))] -- NEW, lock-os köznyezet

data InterpreterError
  = TypeError { message :: String } -- típushiba üzenettel
  | ScopeError { message :: String } -- variable not in scope üzenettel
  | DivByZeroError { message :: String } -- 0-val való osztás hibaüzenettel
  | LockError { message :: String }
  deriving Show


-- Értékeljünk ki egy kifejezést!
evalExp :: (HasCallStack, MonadError InterpreterError m) => Exp -> Env -> m Val
evalExp exp env = case exp of
  IsLocked l -> do 
    case l of
      (Var v) -> case lookup v env of
        (Just (l , e)) -> return $ VBool $ l == ReadWrite || l == Write
        Nothing -> throwError $ ScopeError "Argument to locked is out of scope"
      _     -> throwError $ LockError "Argument to locked has to be a variable" 
  Fallback a b -> do
    case a of
      (Var v) -> case lookup v env of
        (Just (l , e)) -> if l == ReadWrite
          then evalExp b env
          else evalExp a env
        Nothing -> throwError $ ScopeError "First argument to ? is out of scope"
      _     -> throwError $ LockError "First argument to ? has to be a variable" 
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
    Just (l , v) -> if l == ReadWrite 
      then throwError $ LockError $ "Variable " ++ str ++ " is locked"
      else return v
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
      (VLam s env' e) -> evalExp e ((s, (None, v2)) : env')
      _ -> throwError (TypeError $ "Type error in the operands of function application\nSTACK TRACE:\n" ++ stackTrace)


updateEnv :: Env -> String -> (LockType, Val) -> Env
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
  ReadWriteLock s -> do
    env <- get
    case lookup s env of
      Nothing -> throwError (ScopeError $ "Scope error in the operand to rwlock. " ++ s ++ " is not in scope.\nSTACK TRACE:\n" ++ stackTrace)
      Just (l, v') -> put $ updateEnv env s (ReadWrite, v')
  WriteLock s -> do
    env <- get
    case lookup s env of
            Nothing -> throwError (ScopeError $ "Scope error in the operand to wlock. " ++ s ++ " is not in scope.\nSTACK TRACE:\n" ++ stackTrace)
            Just (l, v') -> put $ updateEnv env s (Write, v')
  Unlock s -> do
    env <- get
    case lookup s env of
        Nothing -> throwError (ScopeError $ "Scope error in the operand to unlock. " ++ s ++ " is not in scope.\nSTACK TRACE:\n" ++ stackTrace)
        Just (l, v') -> put $ updateEnv env s (None, v')
  Assign x e -> do
    env <- get
    v <- evalExp e env
    case lookup x env of
      Nothing -> put (updateEnv env x (None, v))
      Just (l , s') -> if l == Write || l == ReadWrite
        then throwError (LockError $ "Lock error : " ++ x ++ " is locked in :\nSTACK TRACE:\n" ++ stackTrace)
        else put (updateEnv env x (None, v))
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

teste1 = run "x := 1" == [("x",(None,VInt 1))]
teste2 = run "x := 1; wlock x" == [("x",(Write,VInt 1))] 
teste3 = run "x := 1; wlock x; x := 2"
teste4 = run "x := 1; wlock x; y := x" == [("x",(Write,VInt 1)),("y",(None,VInt 1))]
teste5 = (snd $ snd $ run "x := 1; rwlock x; y := x" !! 1)
teste6 = run "x := 1; rwlock x; y := locked x" == [("x",(ReadWrite,VInt 1)),("y",(None,VBool True))]
teste7 = run "x := 1; rwlock x; unlock x" == [("x",(None,VInt 1))]
teste8 = run "x := true; rwlock x; while (locked x) do unlock x end" == [("x",(None,VBool True))]
teste9 = run "x := 1; wlock x; y := x ? 2; rwlock y; unlock x; x := y ? 3" == [("x",(None,VInt 3)),("y",(ReadWrite,VInt 1))]