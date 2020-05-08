{-# language DeriveFunctor #-}

import Prelude hiding (exp)
import Control.Monad
import Control.Applicative    -- many, some
import Data.Char -- isSpace, isLetter, isDigit, isAlpha

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)


-- Feladatok
--------------------------------------------------------------------------------

-- A következő a "takeWhile" függvény parser megfelelője, addig olvas
-- egy String-et, amíg egy feltétel igaz a karakterekre.
-- Pl: takeWhileP isDigit kiolvassa az összes számjegyet az input elejéről.
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP f = many (satisfy f)

-- "manyUntil pa pb" addig olvas ismételten (akár nullaszor) pa-t, amíg pb-nincs
-- olvasva.
-- Például: manyUntil (satisfy (const True)) (char 'x') minden karaktert
-- kiolvas az első 'x'-ig, és az 'x'-et is kiolvassa.
manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil pa pb =  (pb >> pure [])
               <|> ((:) <$> pa <*> manyUntil pa pb)

-- mikor jó manyUntil?
-- tipikus: line comment:    -- komment a sor végéig
-- pl:
lineComment :: Parser ()
lineComment = string "--" *> manyUntil anyChar (char '\n') *> pure ()

-- ws parser-t ki lehet egészíteni: kezeli a sor kommenteket
ws' = many (() <$ some (char ' ' <|> char '\n') <|> lineComment)

-- Írj egy egyszerű csv (comma separated values) parsert!
-- A formátum a következő:
--    - az inputban nulla vagy több sor lehet
--    - minden sorban vesszővel elválasztva szerepelnek vagy számok vagy pedig
--      latin betűt tartalmazó szavak
--    - whitespace nem engedett meg az új sorokon kívül
--    - olvassuk eof-al az input végét
--
-- A parser adja vissza az összes sor listáját, ahol (Either Int String) tárolja
-- a szavakat vagy számokat.
-- Példa helyes inputra:
{-
foo,bar,12,31
40,50,60,kutya,macska
-}
isLatinChar :: Char -> Bool
isLatinChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

word :: Parser String
word = some (satisfy isLatinChar)

-- digitToChar, read
number :: Parser Int
number = read <$> some (satisfy isDigit) -- könnyű/csaló verzió

-- két különböző típusú érték parsolása:
numberOrWord :: Parser (Either Int String)
numberOrWord = (Left <$> number) <|> (Right <$> word)

-- eitherP :: Parser a -> Parser b -> Parser (Either a b)
-- eitherP pa pb = (Left <$> pa) <|> (Right <$> pb)

line :: Parser [Either Int String]
line = sepBy numberOrWord (char ',')

csv1 :: Parser [[Either Int String]]
csv1 = sepBy line (char '\n') <* eof

inp = "foo,bar,12,31\n" ++
      "40,50,60,kutya,macska"


-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b
  deriving Show

type Program = [Statement]
type Name = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}       (lokális scope)
  deriving Show

{-
Írj parser-t a fenti While nyelvhez!
A szintaxist a fenti kommentek összegzik, továbbá:
  - mindenhol lehet whitespace tokenek között
  - a Statement-eket egy Prog-ban válassza el ';'
  - Az operátorok erőssége és assszociativitása a következő:
      infixr 2 ||
      infixr 3 &&
      infix  4 ==
      infix  4 <
      infixl 6 +
      infixl 7 *
  - "not" erősebben köt minden operátornál.
  - A kulcsszavak: not, and, while, do, if, end, true, false
  - A változónevek legyenek betűk olyan nemüres sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonosító, viszont "whilefoo" már az!

Példa szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || true
  end;
  z := x
-}

--------------------------------------------------------------------------------

-- 1. whitespace/token

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

-- azonosítók
keywords = ["not", "and", "while", "do", "if", "end", "true", "false"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords    -- elem: prelude-ből
    then empty          -- ALternative instance-ból (parser lib)
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

-- kifejezések (Exp):

--   atom
--   not
-- infixl 7 *
-- infixl 6 +
-- infix  4 == <         nincs associativitás: nem helyes: (x == y == z)
-- infixr 3 &&
-- infixr 2 ||

atomExp :: Parser Exp  -- bal/jobb környezettól függetlenül olvasható
atomExp = (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (char' '(' *> pExp <* char' ')')

pNot :: Parser Exp
pNot = Not <$> (string' "not" *> atomExp)

pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp     -- két operátor ezen az erősségi szinten
pEqOrLt =                 -- nincs láncolás/fold! mivel nem asszociatív operátorok
                          -- házi feladat: adott erősségi szinten több asszociatív
                          --   operátor?
                          -- (tipp: csoportosítani az infixl-eket és az infixr-eket)
      (Eq <$> pAdd <*> (string' "==" *> pAdd))
  <|> (Lt <$> pAdd <*> (string' "<"  *> pAdd))

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr


-- Statement parsolás
pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')


-- házi feladat: csak do notációval megírni a következő parser-t:
--   (nem használunk Applicative metódust)
pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))
