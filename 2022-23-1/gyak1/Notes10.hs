
{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char

-- köv kisfeladat
------------------------------------------------------------

-- Haskell szintaxisú adat beolvasás
--  (mint pl Parser (Maybe (Int, Int)))

-- feladat megoldás
------------------------------------------------------------
-- <[a-z]+>(,<[a-z]+>)*

charRange_ :: Char -> Char -> Parser ()
charRange_ c1 c2 = () <$ satisfy (\c -> c1 <= c && c <= c2)

pWord :: Parser ()
pWord = some_ (charRange_ 'a' 'z')

p :: Parser ()
p = () <$ sepBy1 (char '<' *> pWord *> char '>') (char ',')


------------------------------------------------------------
-- Előadás témát lehet javasolni Teams-en
-- mintavizsga elérhető
-- utolsó 2 gyak: mintavizsga megoldás
-- második házi canvas-on elérhető


-- Parser lib
------------------------------------------------------------

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

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
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


------------------------------------------------------------

-- Whitespace kezelése:
--   1. definiáljuk a ws parser-t: elfogyasztja a whitespace-eket
--   2. minden primitív parser maga után a ws-t hívja meg
--   3. a "top-level" parser kezdésnek hívja meg a ws-t
--      és befejezésnek pedig az eof-ot

ws :: Parser ()
ws = many_ (satisfy isSpace)

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

pPos' :: Parser Int
pPos' = pPos <* ws

-- Olvass be (Int, Int, Int) típusú kifejezéseket
-- Haskell szintaxis szerint!
-- Engedj meg mindenhol whitespace karaktereket!
-- Az Int-ek legyenek csak
-- pozitívok, olvasd be őket a pPos segítségével.
pTripleInt :: Parser (Int, Int, Int)
pTripleInt = do
  char' '('
  n1 <- pPos'
  char' ','
  n2 <- pPos'
  char' ','
  n3 <- pPos'
  char' ')'
  pure (n1, n2, n3)

pTopTripleInt :: Parser (Int, Int, Int)
pTopTripleInt = ws *> pTripleInt <* eof

pNothing :: Parser (Maybe (Int, Int))
pNothing = do
  string' "Nothing"
  pure Nothing

pJust :: Parser (Maybe (Int, Int))
pJust = do
  string' "Just"
  char' '('
  n1 <- pPos'
  char' ','
  n2 <- pPos'
  char' ')'
  pure (Just (n1, n2))

-- Hasonló módon olvass (Maybe (Int, Int)) értékeket.
pMaybePair :: Parser (Maybe (Int, Int))
pMaybePair = ws *> (pNothing <|> pJust) <* eof

-- Ugyanez Applicative szintaxissal:

pNothing2 :: Parser (Maybe (Int, Int))
pNothing2 = Nothing <$ string' "Nothing"

pJust2 :: Parser (Maybe (Int, Int))
pJust2 =

  -- p1 *> p2 *> p3 <* p4      p3 értékét adom vissza
  -- p1 <* p2 <* p3            p1 értékét adom vissza
  Just <$>
   ((,) <$> (char' '(' *> pPos' <* char' ',')
        <*> (pPos' <* char' ')'))

  -- string' "Just"
  -- char' '('
  -- n1 <- pPos'
  -- char' ','
  -- n2 <- pPos'
  -- char' ')'
  -- pure (Just (n1, n2))

pMaybePair2 :: Parser (Maybe (Int, Int))
pMaybePair2 = ws *> (pNothing2 <|> pJust2) <* eof


-- Olvass [[(String, Int)]] típusú adatot a következő szintaxis szerint:
--  - a bemenet 0 vagy több sorból áll
--  - minden sorban legyen vesszővel elválasztva 0 vagy több kulcs-érték pár
--  - egy kulcs-érték pár szintaxisa legyen "kulcs=érték", ahol a kulcs egy nemüres
--    betűsorozat, az érték pedig egy pozitív Int.
--  - whitespace nem megengedett

-- például:
--    bemenet: "x=0,y=1,foo=200\nx=10,y=20"
--    érték:   [[("x", 0), ("y", 1), ("foo", 200)], [("x", 10), ("y", 20)]]

pKeyVal :: Parser (String, Int)
pKeyVal = do
  k <- some (satisfy isLetter)
  char '='
  n <- pPos
  pure (k, n)

-- alternatív Applicative def:
pKeyVal2 :: Parser (String, Int)
pKeyVal2 = (,) <$> (some (satisfy isLetter))
               <*> (char '=' *> pPos)

pLine :: Parser [(String, Int)]
pLine = sepBy pKeyVal (char ',')

pKeyVals :: Parser [[(String, Int)]]
pKeyVals = sepBy pLine (char '\n')


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
--   3. Minden kötési erősséghez definiálunk egy függvényt
--      Mindig a gyengébb függvény hívja a következő erőssebbet

{-
data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp deriving Show

{-
pAtom :: Parser Exp
pAtom = (Lit <$> pPos')
    <|> (char' '(' *> pAddExp <* char' ')')

pMulExp :: Parser Exp
pMulExp = foldr1 Mul <$> sepBy1 pAtom (char' '*')

pAddExp :: Parser Exp
pAddExp = foldr1 Plus <$> sepBy1 pMulExp (char' '+')

pExp :: Parser Exp
pExp = ws *> pAddExp <* eof
-}

-- Library használatával:

pAtom :: Parser Exp
pAtom = (Lit <$> pPos')
    <|> (char' '(' *> pAddExp <* char' ')')

pMulExp :: Parser Exp
pMulExp = rightAssoc Mul pAtom (char' '*')

pAddExp :: Parser Exp
pAddExp = rightAssoc Plus pMulExp (char' '+')

pExp :: Parser Exp
pExp = ws *> pAddExp <* eof

-- algoritmus: "recursive precendence parsing"

-}

------------------------------------------------------------

-- Írj egy parser-t, ami a következő kifejezéseket olvassa:

data Exp
  = IntLit Int        -- pozitív Int
  | Plus Exp Exp      -- e + e
  | Mul Exp Exp       -- e * e
  | Var String        -- nemüres betűsorozat
  | BoolLit Bool      -- true | false
  | Not Exp           -- not e
  | Eq Exp Exp        -- e == e
   deriving Show

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - not alkalmazás
--   - *     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

-- Probléma: változónév és true/false nem egyértelmű
--  Megoldás:
--     1. változónév parser csak akkor sikeres,
--        ha az eredmény nem kulcsszó
--     2. kulcsszó parser csak akkor sikeres,
--        ha egy kulcsszót nem követ változó karakter
--        pl: truefoo egy értelmes azonosítónév

-- "kulcsszó": bármilyen nyelvi elem, ami ütközik
-- a változónevekkel
keywords :: [String]
keywords = ["true", "false", "not"]

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pKeyword' :: String -> Parser ()
pKeyword' s = do
  string s
  -- ha folytatódik az input betűvel, akkor fail
  -- egyébként pedig ws-t olvasunk és sikeresen visszatérünk
  (satisfy isLetter *> empty) <|> ws

-- Kötési erősségek:
--   - atomok: literálok, változók, zárójelezett kifejezések
--   - not alkalmazás
--   - *     : jobbra asszociál
--   - +     : jobbra asszociál
--   - ==    : nem asszociatív

pAtom :: Parser Exp
pAtom = (IntLit <$> pPos')
    <|> (BoolLit True  <$ pKeyword' "true")
    <|> (BoolLit False <$ pKeyword' "false")
    <|> (char' '(' *> pEqExp <* char' ')')
    <|> (Var <$> pIdent')

pNotExp :: Parser Exp
pNotExp = (Not <$> (pKeyword' "not" *> pAtom))
      <|> pAtom

pMulExp :: Parser Exp
pMulExp = rightAssoc Mul pNotExp (char' '*')

pAddExp :: Parser Exp
pAddExp = rightAssoc Plus pMulExp (char' '+')

pEqExp :: Parser Exp
pEqExp = nonAssoc Eq pAddExp (string' "==")

pExp :: Parser Exp
pExp = ws *> pEqExp <* eof

-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- bemenet-kimenet példák:
--    x           TVar "x"
--    y           TVar "y"
--  f x           App (TVar "f") (TVar "x")
--  f x y         App (App (TVar "f") (TVar "x")) (TVar "y")
--  \x. x         Lam "x" (TVar "x")
--  \x y. x       Lam "x" (Lam "y" (TVar "x"))
--  \x. \y. x     Lam "x" (Lam "y" (TVar "x"))
--  (\x. x) y     App (Lam "x" (TVar "x")) (TVar "y")

data Tm = TVar String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
