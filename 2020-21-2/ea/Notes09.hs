
-- precedencia, kulcsszó, azonosító
-- kiértékelés

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State

import Debug.Trace -- trace, traceM


newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor  -- sikeres parse végeredményén függvény alkalmazása

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- nem olvasunk, csak visszaadunk egy értéket
  return a = Parser $ \s -> Just (a, s)

  -- egymás után futtatunk két parser-t (a második függhet az első
  -- eredményétől)
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where

  -- rögtön hibázó parser
  empty = Parser $ \_ -> Nothing

  -- először az első parser-t futtatjuk, hiba esetén a másodikat
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

-- üres input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- feltételnek megfelelő karakter
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- konkrét karakter
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- bármilyen karakter
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét string
string :: String -> Parser ()
string str = mapM_ char str

-- 1 vagy több "a"-t olvasunk, "sep"-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)
                      -- a        [a]
                 -- (:) :: a -> [a] -> [a]

-- 0 vagy több "a"-t olvasunk, "set"-el szeparálva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- + Control.Applicative-ból importálva:
--  - many   : nulla vagy több érték olvasása
--  - some   : egy vagy több érték olvasása
many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- Beolvasunk először egy b-t, utána 0 vagy több a-t, az eredményeket
-- balra asszociálva kombináljuk az adott függvénnyel.
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

string' :: String -> Parser ()
string' str = string str <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pDebug :: String -> Parser ()
pDebug msg = Parser $ \s -> trace (msg ++ "     " ++ s) (Just ((), s))

--------------------------------------------------------------------------------

{-
-- számliterál, +, *, zárójel, ws
--   * erősebb mint +, jobbra asszociálnak az operátorok

-- (AST: abstract syntax tree) (szintaxisfa)

data Exp = IntLit Int | Add Exp Exp | Mul Exp Exp
  deriving (Show)

-- példa helyes inputra:
--   10
--   (20)
--   ((30))
--   10 + 20 + 30
--   10 * 20 + 30
--   10 * (20 + 30)


-- rekurzív precedencia parsolás:
--   minden kötési errőséghez: egy függvény

-- atomi (zárt) kifejezések: olvasás független a bal/jobb kontextustól
--   pl: - literál
--       - két oldalról szimbólummal határolva (pl zárójelezés!)
-- atomi kifejezés: "végtelen" kötési erősség

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

-- precedencia szintek felírása (csökkenő sorrendben):

--   - atom    : literál vagy zárójelezett kifejezés
--   - *       : jobb asszoc
--   - +       : jobb asszoc

-- minden szinthez egy függvény:

pAtom :: Parser Exp
pAtom = pDebug "pAtom" >> ((IntLit <$> posInt') <|> (char' '(' *> pAdd <* char' ')'))

pMul :: Parser Exp
pMul = pDebug "pMul" >> (foldr1 Mul <$> sepBy1 pAtom (char' '*'))

pAdd :: Parser Exp
pAdd = pDebug "pAdd" >> (foldr1 Add <$> sepBy1 pMul (char' '+'))

-- a leggyengébb parser top-level verziója:
pExp :: Parser Exp
pExp = pDebug "pExp" >> (topLevel pAdd)

-- gyengébb függvény hívja az erősebbet rekurzívan
--   pAtom hívhatja újra a leggyengébbet

-- minden parser olvassa az összes erőssebb kifejezést is


-- (2 * 2) * 2
-- pExp
--  pAdd
--   pMul
--    pAtom    2 * 2) * 2
--     pAdd
--      pMul
--       pAtom   * 2) * 2
--      pMul     2) * 2
-- stb.

-- pDebug utasítást helyezzük el különböző pontokon a parser-ben
--

-}

-- kulcsszó/azonosító
--------------------------------------------------------------------------------

-- össze korábbi, "if then else", Bool literál, változónév
--
--   változónév: nemüres betűsorozat
--   Bool literál: True|False
--
--   precedenciák:
--     atom         : szám, Bool literál, (), változónév
--     *            : jobb asszoc
--     +            : jobb asszoc
--     if then else : prefix (jobbra asszociál mindenképpen)

-- if then else : "prefix" operátor  (jobbról nyitott operátor)

-- operátorok: zárt, prefix, postfix, infix (jobb/bal asszoc vagy nem asszoc)
--     (pl nem asszoc infix: (==))
--        0 == 2 == 3 nem helyes Haskell-ben

-- láncolt ifthenelse:
--     if b then 10 else if x == 10 then False else True

-- két oldalról zárt if:
--     if b then 10 else 20 end

data Exp = IntLit Int | Add Exp Exp | Mul Exp Exp | IfThenElse Exp Exp Exp
         | BoolLit Bool | Var String
  deriving (Show)

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

-- pBool' :: Parser Bool
-- pBool' = (True  <$ string' "True") <|> (False <$ string' "False")

-- pVar' :: Parser String
-- pVar' = some (satisfy isLetter) <* ws

-- probléma: többértelműség: True, False, if, then, else  mind lehetnének változónevek is!
-- megoldás: pVar' nem lehet kulcsszó

keywords :: [String]
keywords = ["True", "False", "if", "then", "else"]

pVar' :: Parser String
pVar' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords then
    empty
  else
    pure x

-- pBool' még nem 100%-ig helyes:
-- keyword' segédfüggvényt

-- Control.Applicative
-- optional :: Parser a -> Parser (Maybe a)
-- optional pa = (Just <$> pa) <|> pure Nothing

-- notFollowedBy : kombinátor,
notFollowedBy :: Parser a -> Parser b -> Parser a
notFollowedBy pa pb = do
  a <- pa
  mb <- optional pb
  case mb of
    Nothing -> pure a
    _       -> empty    -- házi feladat: notFollowedBy definíció optional nélkül

keyword' :: String -> Parser ()
keyword' kw = notFollowedBy (string kw) (satisfy isLetter) <* ws

pBool' :: Parser Bool
pBool' = (True  <$ keyword' "True") <|> (False <$ keyword' "False")

pAtom :: Parser Exp
pAtom =  (BoolLit <$> pBool')
     <|> (IntLit <$> posInt')
     <|> (Var <$> pVar')
     <|> (char' '(' *> pIf <* char' ')')

pMul :: Parser Exp
pMul = foldr1 Mul <$> sepBy1 pAtom (char' '*')

pAdd :: Parser Exp
pAdd = foldr1 Add <$> sepBy1 pMul (char' '+')

pIf :: Parser Exp    -- fontos: "fallback" pAdd-re, ha nincsen olvasható if kifejezés
pIf =  (IfThenElse <$> (keyword' "if" *> pIf <* keyword' "then")
                   <*> (pIf <* keyword' "else")
                   <*> pIf)
   <|> pAdd

pIfMonadic :: Parser Exp    -- fontos: "fallback" pAdd-re, ha nincsen olvasható if kifejezés
pIfMonadic =
  do {
    keyword' "if";
    b <- pIf;
    keyword' "then";
    t <- pIf;
    keyword' "else";
    f <- pIf;
    pure $ IfThenElse b t f}
  <|>
  do {
    pAdd}

pExp :: Parser Exp
pExp = topLevel pIf


--------------------------------------------------------------------------------
-- összefoglaló :

{-
1. ws, token parserek
2. kulcsszavak, kulcsszó parser, azonosító parser (változónév)
3. Soroljuk fel a kötési erősség szerint a kifejezés-csoportokat, deklaráljunk minden erősséghez 1 függvényt
  - minden gyengébb függvény hívja a következő erősebbet, minden függvény default esetben próbálja az erősebbeket is olvasni
  - zárt részkifejezés (két oldalról kulcsszó/szimbolum határolja) : leggyengébb kifejezést olvassuk ilyen esetben
     (klasszikus példa: zárójel belseje, de lehet máshol is)
4. Definiáljuk a top level parsert.
-}

-- Házi feladat: függvény:  OpTable -> Parser Exp
-- adott precedenciák, asszociativitások, operátor-szimbolúmok alapján adja vissza a teljes parser-t
