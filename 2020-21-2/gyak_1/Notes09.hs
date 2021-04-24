
{-# LANGUAGE DeriveFunctor #-}

-- BEAD: egyszerű operátor szintaxis, infixLeft/infixRight/infixNonAssoc segítségével megoldható
--    (+ whitespace lehet benne)

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State
import Debug.Trace

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)

  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

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

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

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

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

debug :: String -> Parser ()
debug msg = Parser $ \s -> trace (msg ++ "  input: " ++ s) (Just ((), s))

--------------------------------------------------------------------------------

-- pl. debug használatára
p1 :: Parser ()
p1 = do
  debug "1. foo" >> string' "foo"
  debug "2. foo" >> string' "foo"

p2 :: Parser ()
p2 = many_ (string' "kutya" >> debug "kutya olvasva")

p3 :: Parser ()
p3 = many_ (debug "kutyát próbálunk" >> string' "kutya" >> debug "kutya olvasva")


-- BEAD feladat?
--------------------------------------------------------------------------------

-- tipp: sepBy

pBool :: Parser Bool
pBool = (True <$ string "True")
    <|> (False <$ string "False")

pPair :: Parser (Bool, Bool)
pPair =
  (,) <$> (char '(' *> pBool <* char ',')
      <*> (pBool <* char ')')

-- parens :: Parser a -> Parser a
-- parens pa = char '(' *> pa <* char ')'

-- pPair' :: Parser (Bool, Bool)
-- pPair' = parens ((,) <$> pBool <*> (char ',' *> pBool))

f :: Parser [(Bool, Bool)]
f = char '[' *> sepBy pPair (char ',') <* char ']'

-- monadikus stílusban

pBoolM :: Parser Bool
pBoolM = do {string "True"; pure True}
     <|> do {string "False"; pure False}

pPairM :: Parser (Bool, Bool)
pPairM = do
  char '('
  b1 <- pBoolM
  char ','
  b2 <- pBoolM
  char ')'
  pure (b1, b2)

fM :: Parser [(Bool, Bool)]
fM = do
  char '['
  boolPairs <- sepBy pPairM (char ',')
  char ']'
  pure boolPairs

--------------------------------------------------------------------------------


-- Házi feladat:
-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined



-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))
-- Szóköz és újsor lehet bárhol.


{-

Parser írás lépései ("recursive precedence parsing / recursive operator parsing")

   1. definiáljuk ws-t, alap token parsereket
   2. írjuk fel kötési erősségi sorrendben a kifejezés-csoportokat (csökkenő erősségi sorrendben)
      - atomok : int literál, zárójelezett kifejezés
            (nem függ a bal/jobb környezettől, ált: literál, azonosító, kulcsszó, két oldalról elhatárolt kifejezés)
      - *      : bináris, jobb asszoc
      - +      : bináris, jobb asszoc
      - ==     : bináris, nem asszoc

   3. Minden erősséghez definiálunk egy darab függvényt.
      - Gyengébb függvény hívja az eggyel erősebbet (default eset: megpróbáljuk az egyel erősebbet)

      - Ha van olyan részkifejezés, ami két oldalról token-el határolva van, akkor meghívjuk a leggyengébb parser-t

      - bal/jobb aszociatív infix műveletek: sepBy1 függvénnyel
          jobb asszoc:   foldr1 <konstruktor> <$> sepBy1 <következő parser> <operátor szimbólum parser>
          bal  asszoc:   foldl1 <konstruktor> <$> sepBy1 <következő parser> <operátor szimbólum parser>
          nem  asszoc:   lásd kód, sepBy1 eredményre illesztünk mintát

   4. topLevel-t alkalmazzuk a leggyengébb parser-re

-}

data Exp =
    BoolLit Bool
  | EqInt Exp Exp
  | Lit Int | Plus Exp Exp | Mul Exp Exp
  | Div Exp Exp
  deriving Show

pAtom :: Parser Exp
pAtom = debug "pAtom" *> ((Lit <$> posInt') <|> (char' '(' *> pEqInt <* char' ')')) <* debug "pAtom sikeres"


{-
pMul :: Parser Exp
pMul = debug "pMul" >> (foldr1 Mul <$> sepBy1 pAtom (char' '*'))

-- atomok nemüres sorozata, +-al elválasztva
-- az eredmény foldr1-el (jobb asszoc!) kombinálva
pAdd :: Parser Exp
pAdd = debug "pAdd" >> (foldr1 Plus <$> sepBy1 pMul (char' '+'))

-- non asszoc infix operátor
pEqInt :: Parser Exp
pEqInt = debug "pEqInt" >> do
  exps <- sepBy1 pAdd (string' "==")
  case exps of
    [exp]        -> pure exp                -- 1 db pAdd kifejezés
    [exp1, exp2] -> pure $ EqInt exp1 exp2  -- exp1 == exp2
    _            -> empty                   -- exp1 == exp2 == exp3 ... expN

pExp :: Parser Exp
pExp = debug "pExp" >> topLevel pEqInt
-}


-- faktoráljuk ki a bizonyos infix eseteket!

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
    _            -> empty                     -- exp1 `psep` exp2 `psep` exp3 ... expN

--------------------------------------------------------------------------------

pDiv :: Parser Exp
pDiv = infixLeft pAtom (char' '/') Div

pMul :: Parser Exp
pMul = infixRight pDiv (char' '*') Mul

-- atomok nemüres sorozata, +-al elválasztva
-- az eredmény foldr1-el (jobb asszoc!) kombinálva
pAdd :: Parser Exp
pAdd = infixRight pMul (char' '+') Plus

-- non asszoc infix operátor
pEqInt :: Parser Exp
pEqInt = infixNonAssoc pAdd (string' "==") EqInt

pExp :: Parser Exp
pExp = debug "pExp" >> topLevel pEqInt

-- Nem mindent lehet segédfüggvényekkel megcsinálni
--   - Adott erősségi szinten több különböző operátor?
--   - Prefix/postfix operátor?
--   -     if - then - else -       (prefix operátor 3 db részkifejezés)



-- Írj egy parser-t, ami zárójeleket, +-t, *-t és pozitív Int literálokat tartalmazó kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30 * 1
--           10 + 10 * 20
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))
-- A * operátor kössön erősebben a +-nál, és asszociáljon jobbra.
-- Minden literál és szimbólum között lehet whitespace.

data Exp2 = Lit2 Int | Plus2 Exp2 Exp2 | Mul2 Exp2 Exp2 deriving Show


-- Írj egy parser-t ami a korábbi nyelvet kiegészíti Bool literálokkal,
-- logikai és (&&), vagy (||), egyenlőség-vizsgálat (==) és negáció (not) műveletekkel
--------------------------------------------------------------------------------

-- precendenciák erősség csökkenő sorrendjében:
--   - literálok, zárójeles kifejezés
--   - *  (bal asszoc)
--   - +  (bal asszoc)
--   - && (jobb asszoc)
--   - || (jobb asszoc)
--   - == (nem asszociatív)


data Exp3 = Lit3 Int | Plus3 Exp3 Exp3 | Mul3 Exp3 Exp3
          | BoolLit3 Bool | And Exp3 Exp2 | Or Exp3 Exp3 | Not Exp3 | Eq Exp3 Exp3


-- bónusz: írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
