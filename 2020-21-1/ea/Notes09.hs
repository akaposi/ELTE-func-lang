
-- Precedencia-parsolás, bal rekurzió, bal faktorálás
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)} deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs -> if f c then Just (c, cs) else Nothing
  []   -> Nothing

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \s -> Nothing
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Alternative f => f a -> f ()
many_ p = () <$ many p

some_ :: Alternative f => f a -> f ()
some_ p = () <$ some p

-- precedencia
--------------------------------------------------------------------------------

-- Rekurzív parser-t használunk, akkor a^n b^n megírható Applicative-al is
-- Rekurzió + Applicative parser --> általánosabb mint a regex-ek
--                                   (általános precedencia parsolás)

optional :: Parser a -> Parser (Maybe a)
optional pa = (Just <$> pa) <|> pure Nothing

ab :: Parser ()
ab = (char 'a' >> (ab <|> pure ()) >> char 'b')

-- kiegyensúlyozott zárójelezés (minden nyitáshoz kell egy zárás)
--   (())((()))  OK
--   ()()()      OK
--   (()())      OK

-- min 1 nyitás, kiegyensúlyozott zárójelezés
par :: Parser ()
par = char '(' >> many_ par >> char ')'   -- érdemes vizualizálni a rekurzív hívásokat

-- akárhány nyitás
par' :: Parser ()
par' = par <|> pure ()


------------------------------------------------------------
-- Feladat: +, zárójel, szám literálok, (whitespace lehetséges)
-- + jobbra zárójelez  (pl. a + b + c == a + (b + c))

-- Annak a függvényében, hogy mennyire strukturált adatot akarunk olvasni:
--   1. Parser ()   (csak felismerés)
--   2. Parser Int  (rögtön kiértékelés)
--   3. Parser Exp  (szintaxisfát ad vissza)

-- 1. szintaxisfa definiálása

data Exp = Lit Int | Add Exp Exp deriving (Show)

-- 2. lexikális elemzés (ws + token parserek megadása)

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pIntLit' :: Parser Int
pIntLit' = do
  ds <- some (digitToInt <$> satisfy isDigit) <* ws
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- 3. zárt kifejezések olvasása

-- (10 + 20) + 30
-- (((10 + 20) + 30))
-- ((10))
-- (10 + 20 + 30)

-- Hogyan érdemes kezdeni:
--  Milyen kifejezések "zártak"/atomi kifejezések?
--    balról és jobbról is token határolja el

--  Nem befolyásolja az olvasást, hogy mi áll a kif. előtt/után

--  ellenpélda:   3 + 4      --> Add (Lit 3) (Lit 4)
--                3 + 4 * 5  --> Add (Lit 3) (Mul (Lit 4) (Lit 5))

pAtom :: Parser Exp
pAtom = (Lit <$> pIntLit')
   <|> (char' '(' *> pExp <* char' ')')

-- exp legyen +-al szeparált listája az atomoknak
pExp :: Parser Exp
pExp = foldr1 Add <$> sepBy1 pAtom (char' '+')   -- foldr1 :: (a -> a -> a) -> [a] -> a
    -- foldl1 Add <$> sepBy1 pAtom (char' '+')

-- foldr1 Add (map Lit [0..5]) == Add (Lit 0) (Add (Lit 1) (Add (Lit 2) (Add (Lit 3) (Add (Lit 4) (Lit 5)))))
-- foldl1 Add (map Lit [0..5]) == Add (Add (Add (Add (Add (Lit 0) (Lit 1)) (Lit 2)) (Lit 3)) (Lit 4)) (Lit 5)

pTopExp :: Parser Exp
pTopExp = topLevel pExp

-- "(0 + 1) + 2"

-- rossz megoldás (naiv megoldás)
pExp' :: Parser Exp
pExp' =
      (Add <$> pExp' <*> (char' '+' *> pExp'))
  <|> (char' '(' *> pExp' <* char' ')')
  <|> (Lit <$> pIntLit')


-- "bal rekurzív" függvény: létezik olyan futás, ahol egy parser a legelső olvasási műveletként hívja magát
--                          (végtelen loop)   (nem fogyaszt inputot soha)
-- (probléma elő tud kerülni, ha balra asszociált operátort olvasunk)

-- megoldás: sepBy/many/some függvények haszálata
--           kapunk listát, foldl segítségével balra asszociálunk
--           (library-k: chainl, chainl1, függvények (library: parsec, megaparsec, attoparsec))

-- precedencia, +, *, zárójel, literál,
-- * kössön erősebben, mint a +
-- +, * jobbra zárójelez
--------------------------------------------------------------------------------

-- precendia sorrend:

-- 1. atom       "végtelen" erősség
-- 2. *          jobbra asszoc
-- 3. +          jobbra asszoc


-- Gyengébb parser hívja az erősebbet
-- Atom hívja a leggyengébbet
-- (körbe megy a rekurzív hívás: gyengébb hívja 1-el erősebbet, atom hívja a leggyengébbet)

data Exp2 = Lit2 Int | Add2 Exp2 Exp2 | Mul2 Exp2 Exp2 deriving (Show)

pAtom2 :: Parser Exp2
pAtom2 = (Lit2 <$> pIntLit')
   <|> (char' '(' *> pAdd2 <* char' ')')

pMul2 :: Parser Exp2
pMul2 = foldr1 Mul2 <$> sepBy1 pAtom2 (char' '*')

pAdd2 :: Parser Exp2
pAdd2 = foldr1 Add2 <$> sepBy1 pMul2 (char' '+')

-- topLevel <leggyengébb kifejezés>
pTopExp2 :: Parser Exp2
pTopExp2 = topLevel pAdd2


-- "1 * 2 + 3"

-- példa: bináris operátor ami nem asszociál semerre
------------------------------------------------------------

-- 1. atom (int/bool lit, zárójelezés)
-- 2. *  (jobb asszoc)
-- 3. +  (jobb asszoc)
-- 4. == (nem asszoc)          -- (10 == 20 == 30) nem helyes

data Exp3
  = IntLit3 Int
  | Add3 Exp3 Exp3
  | Mul3 Exp3 Exp3
  | Eq3 Exp3 Exp3
  | BoolLit3 Bool
  deriving (Show)

pAtom3 :: Parser Exp3
pAtom3 =
       (IntLit3 <$> pIntLit')
   <|> (BoolLit3 <$> ((True <$ string' "true") <|> (False <$ string' "false")))
   <|> (char' '(' *> pEq' <* char' ')')

pMul3 :: Parser Exp3
pMul3 = foldr1 Mul3 <$> sepBy1 pAtom3 (char' '*')

pAdd3 :: Parser Exp3
pAdd3 = foldr1 Add3 <$> sepBy1 pMul3 (char' '+')


-- Ha a jelenlegi operátort nem tudjuk olvasni, akkor tovább esünk a következő erősebb Parser-re!
-- A sepBy1 alapból kezeli a tovább esést, viszont itt nekünk kell ezt kezelni.
pEq :: Parser Exp3
pEq = (Eq3 <$> pAdd3 <*> (string' "==" *> pAdd3)) <|> pAdd3


-- pEq nem hatékony: pAdd olvasás után, ha "==" nem sikerül, akkor újra pAdd-et olvas (exponenciális worst case!)
-- javítás: bal faktorálás ("left factoring")
-- általánosan:   (pa >> pb) <|> (pa >> pc)   --->    pa >> (pb <|> pc)
pEq' :: Parser Exp3
pEq' = do
  e1 <- pAdd3
  (Eq3 e1 <$> (string' "==" *> pAdd3)) <|> pure e1

-- pEq' : lineáris idejű parsolás
--        (memóriában is lineáris, rekurzív hívás olyan mély lehet, mint a kifejezés mérete)

-- topLevel <leggyengébb kifejezés>
pTopExp3 :: Parser Exp3
pTopExp3 = topLevel pEq'


-- (real-world compiler parser: clang, gcc, msvc) mind rekurzív precedencia parsolás
--    (alapból rekurzív lesszállás + precedencia, ahol a sebesség kritikus ott speciálisabb operátor parsolás)
-- (kivételek: GHC, Python: parser generátor)
