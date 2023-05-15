{-# language DeriveFunctor, InstanceSigs, DeriveFoldable #-}

import Control.Monad
import Control.Applicative
import Data.Char


------------------------------------------------------------
-- Köv Kisfeladat:
--   Haskell szintaxisú adat olvasás
--    (pár, Maybe, lista, stb.)

------------------------------------------------------------


-- Megoldás

-- Írj parser-t, ami megfelel a következő regex-nek:
--   <[a-z]+>(,<[a-z]+>)*
-- Nem muszáj az input végére illeszkeni.

lowercase =
  satisfy (\c -> 'a' <= c && c <= 'z')

kisf :: Parser ()
kisf = char '<' *> some_ lowercase *> char '>' *>
       many_ (string ",<" *> some_ lowercase *> char '>')

-- sepBy1-el
kisf' :: Parser ()
kisf' = () <$ sepBy1 (char '<' *> some_ lowercase *> char '>')
                     (char ',')

-- kód duplikáció Monad/Applicative:
--    (>>)    (*>)
--    return  pure

-- idiomatikus: Applicative-ot preferáljuk

-- Library
--------------------------------------------------------------------------------

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

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- (p1 <|> p2) először p1-et futtatja, ha az hibázik, akkor p2-t.
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- Sikeres az üres inputon
eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- Olvassunk egy karaktert, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- Olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- Olvassunk egy konkrét string-et az input elejéről
string :: String -> Parser ()
string = mapM_ char

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

-- Operátorok

-- Jobb asszociatív infix:
-- Bemenetek: (a -> a -> a)  -- függvény, amivel az eredényt kombináljuk
--         -> Parser a       -- milyen kifejezés van az operátorok között
--         -> Parser sep     -- operátor olvasása
--         -> Parser a
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

-- Bal asszociatív infix
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

-- Nem asszociatív infix
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

-- Nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa


-- Whitespace, strukturált adat olvasás
------------------------------------------------------------

-- Whitespace-ek szerepelhetnek bárhol
-- Hogyan kezeljük:
--   1. Definiáljuk "ws"-t, ami whitespace-t olvas
--   2. Minden alapvető parser (ami ténylegesen karaktert olvas)
--      maga után hívja meg ws-t
--   3. Cél parser ("top-level parser") futtatása:
--      topLevel kombinátor segítségével

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

char' :: Char -> Parser ()
char' c = char c >> ws

string'  :: String -> Parser ()
string' s = string s >> ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

pPos' :: Parser Int
pPos' = pPos <* ws


-- Haskell szintaxis szerint olvass be Maybe (Int, Int) típusú értékeket!
-- Whitespace mindenhol megengedett.

pMaybePair :: Parser (Maybe (Int, Int))
pMaybePair =
  (do string' "Nothing"
      return Nothing)
  <|>
  (do string' "Just"
      char' '('
      n1 <- pPos'
      char' ','
      n2 <- pPos'
      char' ')'
      return (Just (n1, n2)))

-- Applicative
pMaybePair' :: Parser (Maybe (Int, Int))
pMaybePair' =
      (Nothing <$ string' "Nothing")
  <|> (Just <$> ((,) <$> (string' "Just" *> char' '(' *> pPos' <* char' ',')
                     <*> (pPos' <* char' ')')))

-- Hasonló módon, Haskell szintaxis szerint olvass [Int] értékeket
-- Tipp: sepBy függvényt használj
pIntList :: Parser [Int]
pIntList = do
  char' '['
  ns <- sepBy pPos' (char' ',')
  char' ']'
  return ns

pIntList' :: Parser [Int]
pIntList' = char' '[' *> sepBy pPos' (char' ',') <* char' ']'


-- Olvass [[(String, Int)]] típusú adatot a következő szintaxis szerint:
--  - a bemenet 0 vagy több sorból áll
--  - minden sorban legyen vesszővel elválasztva 0 vagy több kulcs-érték pár
--  - egy kulcs-érték pár szintaxisa legyen "kulcs=érték", ahol a kulcs egy nemüres
--    betűsorozat, az érték pedig egy pozitív Int.
--  - whitespace nem megengedett

-- például:
--    bemenet: "x=0,y=1,foo=200\nx=10,y=20"
--    érték:   [[("x", 0), ("y", 1), ("foo", 200)], [("x", 10), ("y", 20)]]
pKeyVals :: Parser [[(String, Int)]]
pKeyVals = undefined


-- Operátorok
--------------------------------------------------------------------------------

-- Írj egy parser-t, ami zárójeleket,
-- +-t, *-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! Whitespace-t mindenhol engedj meg.
------------------------------------------------------------
--
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--           10 * 20 + 5
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3))).
-- A * operátor szintán jobbra asszociáljon, és kössön erősebben, mint a +.

-- input: 10 * 10 + 20
-- output: Plus (Mul (Lit 10) (Lit 10)) (Lit 20)
--   (* erősebb mint a +)

-- Operátorok kezelése:
--   1. Soroljuk fel a kötési erősségeket (csökkenő sorrendben)
--   2. A legerősebb az "atom", olyan kifejezések, amelyek
--      nem függenek a bal/jobb környezettől
--      pl: literál, kulcsszó, zárójelezett kifejezés
--   3. Minden kötési erősséghez definiálunk egy függvényt.
--      Mindig a gyengébb függvény hívja a következő erősebbet.

data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp deriving Show

-- Lit 10                                10
-- Plus (Lit 0) (Lit 2)                  0 + 2
-- Plus (Lit 0) (Mul (Lit 3) (Lit 5))    0 + 3 * 5
-- Mul (Plus (Lit 0) (Lit 1)) (Lit 3)    (0 + 1) * 3

-- három erősség:
--   1. atom: literál, zárójelezett kifejezés
--   2. (*)   jobbra asszociál
--   3. (+)   jobbra asszociál

pAtom :: Parser Exp
pAtom = (Lit <$> pPos') <|> (char' '(' *> pPlus <* char' ')')
    -- leggyengébb parser bármit tud olvasni
    -- zárójel belsejében azt hívjuk meg

-- szorzás kifejezés: 1 vagy több atom, *-al elválasztva
-- végeredmény: jobbra asszociált Mul alkalmazás
pMul :: Parser Exp
pMul = foldr1 Mul <$> sepBy1 pAtom (char' '*')

-- összeadás kifejezés: 1 vagy több szorzás kifejezés, +-al elválasztva
pPlus :: Parser Exp
pPlus = foldr1 Plus <$> sepBy1 pMul (char' '+')

-- topLevel parser: leggyengébb függvény
pExp :: Parser Exp
pExp = topLevel pPlus


-- (library függvényekkel:)
------------------------------------------------------------

-- rightAssoc használat:
-- Bemenetek: (a -> a -> a)  -- függvény, amivel az eredényt kombináljuk
--         -> Parser a       -- milyen kifejezés van az operátorok között
--         -> Parser sep     -- operátor olvasása
--         -> Parser a

-- library függvényekkel ugyanez:
pMul' :: Parser Exp
pMul' = rightAssoc Mul pAtom (char' '*')

pPlus' :: Parser Exp
pPlus' = rightAssoc Plus pMul' (char' '+')

-- Általánosan
--------------------------------------------------------------------------------

-- Van: atom, N darab operátor, erősségi sorrendben
--      mindegyik operátor lehet: jobb asszoc, bal asszoc, nem asszoc, prefix

-- rightAssoc
-- leftAssoc
-- nonAssoc
-- prefix

data Exp2
  = Lit2 Bool         -- true|false
  | Not2 Exp2         -- not e             (prefix, nem láncolható)
  | And2 Exp2 Exp2    -- e && e            (jobb asszoc)
  | Or2 Exp2 Exp2     -- e || e            (jobb asszoc)
  deriving Show
-- Zárójel és whitespace megengedett

pAtom2 :: Parser Exp2
pAtom2 =
     (Lit2 True  <$ string' "true")
 <|> (Lit2 False <$ string' "false")
 <|> (char' '(' *> pOr2 <* char' ')')

pNot2 :: Parser Exp2
pNot2 = prefix Not2 pAtom2 (string' "not")

pAnd2 :: Parser Exp2
pAnd2 = rightAssoc And2 pNot2 (string' "&&")

pOr2 :: Parser Exp2
pOr2 = rightAssoc Or2 pAnd2 (string' "||")

pExp2 :: Parser Exp2
pExp2 = topLevel pOr2


-- Kulcsszavak és azonosítók
--------------------------------------------------------------------------------

-- Írj egy parser-t, ami a következő kifejezéseket olvassa:


data Exp3
  = IntLit3 Int        -- pozitív Int
  | Plus3 Exp3 Exp3    -- e + e
  | Mul3 Exp3 Exp3     -- e * e
  | Var3 String        -- nemüres betűsorozat
  | BoolLit3 Bool      -- true | false
  | Not3 Exp3          -- not e
  | Eq3 Exp3 Exp3      -- e == e
   deriving Show

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - not   : prefix
--   - *     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

-- kulcsszó: token, ami ütközhet változónevekkel
keywords :: [String]
keywords = ["true", "false", "not"]

-- olvassunk egy konkrét kulcsszót
keyword :: String -> Parser ()
keyword s = do
  string s
  m <- optional (satisfy isLetter)
  case m of
    Just _ -> empty
    _      -> ws

-- olvassunk egy azonosítót (változónevet)
ident :: Parser String
ident = do
  s <- some (satisfy isLetter) <* ws
  if elem s keywords
    then empty
    else pure s
