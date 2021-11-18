
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable,
    DeriveTraversable #-}

{-# options_ghc -Wincomplete-patterns #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

-- PARSER LIBRARY
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where

  -- nem dob hibát + nem fogyaszt inputot
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- egymás után két parsert hívunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- parserek közötti választás
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- üres String olvasása
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- Char olvasása, amire egy feltétel teljesül
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ f = () <$ satisfy f

-- konkrét Char olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- parser hívásakor kiír egy String üzenetet
debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- bármilyen Char olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét String-et próbál olvasni
string :: String -> Parser ()
string = traverse_ char

-- Control.Applicative-ból (iterálás):
-- many :: Parser a -> Parser [a]        -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]        -- 1-szer vagy többször olvasás

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

-- whitespace elfogyasztása
ws :: Parser ()
ws = many_ (satisfy isSpace)

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

--------------------------------------------------------------------------------

-- mai + 3 alkalom
--   ma : befejezzük a parsolást + interpretert
--   - vizsgához hasonló interpreter + parser
--   - korábbi vizsgasort megnézni
--   - custom téma (lehet javasolni)
--   - + kimaradt EA: custom téma (lehet javasolni)

-- 2. házi: ezen a héten felteszem
-- 3. házi: következő héten

-- vizsga: 2 óra, feladmegoldás,
--   50-55%: kisfeladat (Functor-tól témák, parser már nincs benne)
--   maradék: kiegészíteni egy parsert+interpretert
--   4 vizsga, vizsg időszak 1. hét első vizsga

-- operátor, kulcsszó, azonosító
-- kifejezésnyelv parse + interp
-- data Exp = IntLit Int | Add Exp Exp | Mul Exp Exp
--   | IfThenElse Exp Exp Exp | BoolLit Bool | Var String

--------------------------------------------------------------------------------

infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp                  -- 1 db pa kifejezés
    [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
    _            -> empty

-- mi az, amire nem elég a fenti API?
--    - adott erősségi szinten több különböző operátor
--    - pl: op1/op2 is jobb asszoc, azonos erősség: e1 op1 e2 op2 e3 op1 e4  helyes

-- parser library-k (parsec): egy "táblaként" kapja egy függvény a
-- precedenciákat és parsereket, és az összes operátort egy függvény
-- parsolja. (opcionális házi)

--------------------------------------------------------------------------------


{-
{-
Precedenciák:
  - literál, zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)                (nem helyes: 10 == 20 == 30)
-}

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp | Eq Exp Exp
  deriving (Eq, Show)

-- 1. token parserek
char' c = char c <* ws
string' s = string s <* ws

posInt' :: Parser Int
posInt' = read <$> (some (satisfy isDigit) <* ws)

-- 2. precedencia parser függvények
atom :: Parser Exp
atom = (Lit <$> posInt')
   <|> (char' '(' *> eqExp <* char' ')')

mulExp :: Parser Exp
mulExp = infixRight atom (char' '*') Mul

addExp :: Parser Exp
addExp = infixRight mulExp (char' '+') Add

eqExp :: Parser Exp
eqExp = infixNonAssoc addExp (string' "==") Eq

parseExp :: Parser Exp
parseExp = topLevel eqExp
-}


-- kulcsszavak + azonosítók
--------------------------------------------------------------------------------

{-
{-
Precedenciák:
  - literál, zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

data Exp
  = IntLit Int
  | BoolLit Bool
  | IfThenElse Exp Exp Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Var String            -- változónév
  deriving (Eq, Show)

-- 1. token parserek
char' c = char c <* ws
string' s = string s <* ws

posInt' :: Parser Int
posInt' = read <$> (some (satisfy isDigit) <* ws)

-- Azonosítók vs kulcsszavak
--   nem szeretnénk összekeverni kulcszót azonosítóval
--   furcsa : \ if then -> if if then then else 0
--   diszjunkt legyen a két parser (azonosító nem lehet kulcsszó és fordítva)

keywords :: [String]
keywords = ["if", "then", "else"]

-- ki akarjuk zárni:   iffoo olvasása: "if" először, utána "foo" mint azonsító
--                     helyett       : "iffoo" egyben mint azonosító
keyword' :: String -> Parser ()
keyword' str = do
  string str
  x <- many (satisfy isLetter) <* ws
  case x of
    "" -> pure ()
    _  -> empty

ident' :: Parser String
ident' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords then empty
                     else pure x

-- 2. precedencia parser függvények
atom :: Parser Exp
atom = (IntLit <$> posInt')
   <|> (char' '(' *> ifThenElse <* char' ')')
   <|> (BoolLit <$> (True  <$ keyword' "true"))
   <|> (BoolLit <$> (False <$ keyword' "false"))
   <|> (Var <$> ident')

mulExp :: Parser Exp
mulExp = infixRight atom (char' '*') Mul

addExp :: Parser Exp
addExp = infixRight mulExp (char' '+') Add

eqExp :: Parser Exp
eqExp = infixNonAssoc addExp (string' "==") Eq

ifThenElse :: Parser Exp
ifThenElse =
  (do
    keyword' "if"
    e1 <- ifThenElse
    keyword' "then"
    e2 <- ifThenElse
    keyword' "else"
    e3 <- ifThenElse
    pure $ IfThenElse e1 e2 e3)
  <|>
   eqExp

parseExp :: Parser Exp
parseExp = topLevel ifThenElse

-- kifejezés interpreter
--------------------------------------------------------------------------------

-- változók kezelése: környezet, ami minden változónévhez értéket rendel

-- értékek
type Val = Either Int Bool

-- környezet
type Env = [(String, Val)]

-- dobhat hibát (típushiba / scope hiba)
eval :: Env -> Exp -> Val
eval env e = case e of

  Var x -> case lookup x env of
    Nothing -> error ("name not in scope: " ++ x)
    Just v  -> v

  IntLit n  -> Left n
  BoolLit b -> Right b

  IfThenElse e1 e2 e3 -> case eval env e1 of
    Right b -> if b then eval env e2 else eval env e3
    Left _  -> error "type error"

  Add e1 e2 -> case (eval env e1, eval env e2) of
    (Left n1, Left n2) -> Left (n1 + n2)
    _                  -> error "type error"

  Mul e1 e2 -> case (eval env e1, eval env e2) of
    (Left n1, Left n2) -> Left (n1 * n2)
    _                  -> error "type error"

  Eq e1 e2 -> case (eval env e1, eval env e2) of
    (Left n1, Left n2)   -> Right (n1 == n2)
    (Right b1, Right b2) -> Right (b1 == b2)
    _                    -> error "type error"

p1 :: String
p1 = "if x == y then y + 10 else x * 20"

run :: Env -> String -> Val
run env str = case runParser parseExp str of
  Nothing     -> error "parse error"
  Just (e, _) -> eval env e
-}


-- Értékadás mint új változó felvétel + mutáció
--------------------------------------------------------------------------------

{-
Precedenciák:
  - literál, zárójel
  - *  (jobb asszoc)
  - +  (jobb asszoc)
  - == (nem asszoc)
  - if e1 then e2 else e3   (prefix)
-}

data Exp
  = IntLit Int
  | BoolLit Bool
  | Add Exp Exp
  | Mul Exp Exp
  | Eq Exp Exp
  | Var String            -- változónév
  deriving (Eq, Show)

type Program = [Statement]

data Statement
  = Assign String Exp               --   x := exp
  | IfThenElse Exp Program Program  --   if e1 then p1 else p2 end
  | While Exp Program               --   while e do p end
  deriving (Eq, Show)

-- legyen minden statement ;-vel elválasztva
-- cél Program-ot olvassunk


-- 1. token parserek
char' c = char c <* ws
string' s = string s <* ws

posInt' :: Parser Int
posInt' = read <$> (some (satisfy isDigit) <* ws)

-- Azonosítók vs kulcsszavak
--   nem szeretnénk összekeverni kulcszót azonosítóval
--   furcsa : \ if then -> if if then then else 0
--   diszjunkt legyen a két parser (azonosító nem lehet kulcsszó és fordítva)

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "end"]

-- ki akarjuk zárni:   iffoo olvasása: "if" először, utána "foo" mint azonsító
--                     helyett       : "iffoo" egyben mint azonosító
keyword' :: String -> Parser ()
keyword' str = do
  string str
  x <- many (satisfy isLetter) <* ws
  case x of
    "" -> pure ()
    _  -> empty

ident' :: Parser String
ident' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords then empty
                     else pure x

-- kifejezések
--------------------------------------------------------------------------------

atom :: Parser Exp
atom = (IntLit <$> posInt')
   <|> (char' '(' *> eqExp <* char' ')')
   <|> (BoolLit <$> (True  <$ keyword' "true"))
   <|> (BoolLit <$> (False <$ keyword' "false"))
   <|> (Var <$> ident')

mulExp :: Parser Exp
mulExp = infixRight atom (char' '*') Mul

addExp :: Parser Exp
addExp = infixRight mulExp (char' '+') Add

eqExp :: Parser Exp
eqExp = infixNonAssoc addExp (string' "==") Eq

-- statement olvasás
--------------------------------------------------------------------------------

pIf     = keyword' "if"
pThen   = keyword' "then"
pElse   = keyword' "else"
pWhile  = keyword' "while"
pDo     = keyword' "do"
pEnd    = keyword' "end"
pAssign = string' ":="

statement :: Parser Statement
statement =
      (Assign <$> (ident' <* pAssign) <*> eqExp)
  <|> (IfThenElse <$> (pIf *> eqExp)
                  <*> (pThen *> program)
                  <*> (pElse *> program <* pEnd))
  <|> (While <$> (pWhile *> eqExp <* pDo)
             <*> (program <* pEnd))

program :: Parser Program
program = sepBy statement (char' ';')

parseProgram :: Parser Program
parseProgram = topLevel program

p1 :: String
p1 = unlines [
  "x := 100;",
  "y := 1000;",
  "while x == 0 do",
    "y := y + 1",
  "end"
  ]

--------------------------------------------------------------------------------
