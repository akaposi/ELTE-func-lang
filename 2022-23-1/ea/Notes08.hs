
-- Parser: whitespace/token parsolás, precedencia
------------------------------------------------------------

{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Data.Char  -- isDigit, isAlpha, digitToInt

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


-- Whitespace kezelése
------------------------------------------------------------

ws :: Parser ()
ws = many_ (satisfy (\c -> c == ' ' || c == '\n'))  -- vagy: Data.Char.isSpace

-- pList :: Parser a -> Parser [a]
-- pList pa = do
--   char '['
--   as <- sepBy pa (char ',')
--   char ']'
--   pure as

-- megoldás:
--   1. definiáljuk a "primitív" parsereket, amelyek ténylegesen
--      olvasnak karaktereket. Mindegyik ilyen parser olvasson ws-t
--      maga után. (Ilyen parserek: "token" parserek)
--       p1 >> p2
--       p1 >> ws >> p2 >> ws
--   2. Egész ("top level") parser ws olvasással kezd és eof-al végez
--

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Ebben a fájlban: token parserek '-s nevűek legyenek

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

pPos' :: Parser Int
pPos' = pPos <* ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

pList :: Parser a -> Parser [a]
pList pa = do
  char' '['
  as <- sepBy pa (char' ',')
  char' ']'
  pure as

pTopList :: Parser a -> Parser [a]
pTopList pa = topLevel (pList pa)


-- Azonosító nevek vs. kulcsszavak
------------------------------------------------------------

{-

-- kifejezésnyelv:

data Exp = Var String | Add Exp Exp | Mul Exp Exp | IntLit Int
  deriving Show

-- szintaxis: ugyanaz, mint Haskell szintaxis,
--  - viszont változóneveket Var nélkül írhatunk
--  - Int literált is konstruktor nélkül írhatunk
--  - változónév: nemüres betűből álló String

-- példa: Add 10 20
--        Mul (Add 10 y) 20
--        Mul (Add foo bar) 100

-- legális-e:  Add Add Add   (két "Add" változónév)

-- kulcsszó: "Add", "Mul"
-- változónevek
-- kettő között nem lehet átfedés:
--    változónév nem lehet kulcsszó
--    változónévnek lehet kulcsszó a prefixe
--      (pl : Addfoo egy változónév)

keywords :: [String]
keywords = ["Add", "Mul"]

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pAdd' :: Parser ()
pAdd' = do
  string "Add"
  (satisfy isLetter >> empty) <|> ws

pMul' :: Parser ()
pMul' = do
  string "Mul"
  (satisfy isLetter >> empty) <|> ws

pExp :: Parser Exp
pExp = (Var <$> pIdent')
   <|> (IntLit <$> pPos')
   <|> (Add <$> (char' '(' *> pAdd' *> pExp) <*> (pExp <* char' ')'))
   <|> (Mul <$> (char' '(' *> pMul' *> pExp) <*> (pExp <* char' ')'))

pTopExp :: Parser Exp
pTopExp = topLevel pExp

-}

-- precedencia, operátorok
------------------------------------------------------------

-- infix operátorok, bal/jobb/nincs asszociativitás
-- tegyük fel: minden operátor kötési erősséges különöző

data Exp = Var String | Add Exp Exp | Mul Exp Exp | IntLit Int
  deriving Show

-- - legyen +, * infix operátor, mindkettő jobbra asszociál
-- - legyen * kötése erősebb mint +
-- - legyen zárójelezés megengedett

pIdent' :: Parser String
pIdent' = some (satisfy isLetter) <* ws

-- megoldás sémája:
--   - minden erősségi szinthez egy függvény
--   - csökkenő erősségi sorrendben
--   - gyenge függvény hívja az egyel erősebbet
--   - "végtelen" erősség: "atomi" parser, olyan
--     parser, hogy nem függ a kétoldali környezetétől
--     pl: azonosító, literál, kulcsszó, [...], (...)

-- erősségek:
--    - atom: int literál, azonosító, zárojelezett kifejezés
--    - * jobb asszoc
--    - + jobb asszoc

pAtom :: Parser Exp
pAtom = (Var <$> pIdent')
    <|> (IntLit <$> pPos')
    <|> (char' '(' *> pAdd <* char' ')')  -- zárójel belseje: legyengyébb parser

pMul :: Parser Exp         -- 1 vagy több *-al elválasztott atom
pMul = foldr1 Mul <$> sepBy1 pAtom (char' '*')

pAdd :: Parser Exp         -- 1 vagy több +-al elválasztott pMul
pAdd = foldr1 Add <$> sepBy1 pMul (char' '+')

pTopExp :: Parser Exp
pTopExp = topLevel pAdd

-- "algoritmus": "recursive precedence parsing"
--               call stack-en

-- operátor parserek általánosan:

--             konstruktor    következő parser      operátor
rightAssoc :: (a -> a -> a) -> Parser a            -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

--             konstruktor    következő parser      operátor
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

-- nem asszociatív infix operátor: nem lehet láncolni
--   példa: == operátor Haskell-ben

--           konstruktor    következő parser    operátor
nonAssoc :: (a -> a -> a) -> Parser a      ->   Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e         -- olvastunk 0 db operátor alkalmazást
    [e1,e2]  -> pure (f e1 e2) -- olvastunk 1 db operátor alkalmazást
    _        -> empty          -- olvastunk 2 vagy több operátor alkalmazást

-- korább pMul és pAdd-et definiálni:

{-
pMul :: Parser Exp
pMul = rightAssoc Mul pAtom (char' '*')

pAdd :: Parser Exp
pAdd = rightAssoc Add pMul (char' '+')
-}

-- Extra: több operátor ugyanazon a precendencia szinten?
------------------------------------------------------------

-- kézzel írjuk:

data Exp2 = Var2 String | Add2 Exp2 Exp2 | Subtract Exp2 Exp2 | IntLit2 Int
  deriving Show

-- ugyanaz a - és + erőssége, jobb asszociatív

pAtom2 :: Parser Exp2
pAtom2 = undefined

pAddSubtract :: Parser Exp2
pAddSubtract = do {
  e <- pAtom2;

      (Add2 e     <$> (char' '+' *> pAddSubtract))
  <|> (Subtract e <$> (char' '-' *> pAddSubtract))
  <|> pure e
  }

-- mi van, ha - és + is balra asszociál?
--  probléma: "left recursion" (bal rekurzió)
--  p1 = p1 <|> p2 <|> p3
--  precízen: bal rekurzió: úgy csinálunk rekurzív hívást,
--   hogy közben az input változatlan

go :: Exp2 -> Parser Exp2
go e =
      (do {char' '+'; e2 <- pAtom2; go (Add2 e e2)})
  <|> (do {char' '-'; e2 <- pAtom2; go (Subtract e e2)})
  <|> pure e

-- megoldás: felveszünk akkumuláló segédfüggvényt
pAddSubtract' :: Parser Exp2
pAddSubtract' = do e <- pAtom2
                   go e

-- 10 + 20 + 30
-- ((10 + 20) + 30)

-- foo = let x = 100

-- bar :: Int
-- bar = 300
