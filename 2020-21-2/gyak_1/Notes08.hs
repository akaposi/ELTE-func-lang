
{-# LANGUAGE DeriveFunctor #-}

-- BEAD: egyszerűbb parser, ami nem ()-ot ad vissza, hanem strukturáltabb adatot
--       pl. Parser (Int, Bool)
--       pl. Parser [Maybe Int]
--       (nem lesz whitespace)

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State

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

-- + Control.Applicative-ból importálva:
--  - many   : nulla vagy több érték olvasása
--  - some   : egy vagy több érték olvasása

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- BEAD feladat megoldás
--------------------------------------------------------------------------------

word = some_ (satisfy isLower)

angled :: Parser a -> Parser a
angled pa = char '<' *> pa <* char '>'

parens :: Parser a -> Parser a
parens pa = char '(' *> pa <* char ')'

-- (<[a-z]+=[0-9]+>)*$

p :: Parser ()
p = many_ (angled (word >> char '=' >> some_ (satisfy isDigit))) >> eof

-- <[a-z]+>(,<[a-z]+>)*

p' :: Parser ()
p' = angled word >> many_ (char ',' >> angled word)

-- implementáld a következő regex-eket:
--------------------------------------------------------------------------------

-- (ac|bd)*
p01 :: Parser ()
p01 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p02 :: Parser ()
p02 = undefined

--------------------------------------------------------------------------------

-- olvass be nulla vagy több ' '-t vagy '\n'-t!
-- "whitespace"
ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

-- -- Olvass be egy számjegyet!
-- digit :: Parser Int
-- digit = do
--   c <- anyChar
--   case c of
--     '0' -> pure 0
--     '1' -> pure 1
--     '2' -> pure 2
--     '3' -> pure 3
--     '4' -> pure 4
--     '5' -> pure 5
--     '6' -> pure 6
--     '7' -> pure 7
--     '8' -> pure 8
--     '9' -> pure 9
--     _   -> empty

-- Olvass be egy számjegyet!
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit
          -- satisfy isDigit                 :: Parser Char
          -- digitToInt <$> satisfy isDigit  :: Parser Int
          -- digitToInt :: Char -> Int

digit' :: Parser Int
digit' = do
  d <- satisfy isDigit
  pure $ digitToInt d

-- Applicative parsolás: (<$>), (<$), (<*>), (<*), (*>)
--    Applicative: "deklaratív" / közeli szintaxis a nyelvtan-specifikációkhoz
--    Monad      : "imperatív"

digitsToInt :: [Int] -> Int
digitsToInt ds =
  sum $ zipWith (*) (reverse ds) (iterate (*10) 1)    -- összes közbülső lista eltüntethető-e?

digitsToInt' :: [Int] -> Int
digitsToInt' ds =
  fst $ foldr (\d (res, place) -> (res + d*place, place*10)) (0, 1 :: Int) ds

digitsToInt'' :: [Int] -> Int
digitsToInt'' ds = fst $ go ds where

  go :: [Int] -> (Int, Int)
  go []     = (0, 1)
  go (d:ds) = case go ds of
    (res, place) -> (res + d*place, place*10)


-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
-- Pl. runParser posInt "100" == Just (100, "")
posInt :: Parser Int
posInt = do
  ds <- some digit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)


-- szeparált lista parsolás:
--  sepBy   pa psep        -- 0 vagy több pa, psep-el választva
--  sepBy1  pa psep        -- 1 vagy több pa, psep-el választva

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Minden literál és szimbólum között lehet whitespace.
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"

-- intList :: Parser [Int]
-- intList = char '[' *> sepBy posInt (char ',') <* char ']'

-- token konvenció: whitespace-ek jó kezelése
--    minden alap parser (ami ténylegesen karaktert olvas) maga után ws-t olvas
--    token parser: alap parser, maga után ws-t olvas

-- minden alap parserből token verziót definiálunk:

char' :: Char -> Parser ()
char' c = char c <* ws

posInt' :: Parser Int
posInt' = posInt <* ws

intList :: Parser [Int]
intList = topLevel (char' '[' *> sepBy posInt' (char' ',') <* char' ']')

-- mivel minden token parser maga után olvas ws-t, a legelső token előtt nem olvasódik ws.
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- Összefoglaló:
--    1. definiáljuk a ws-t     (kommentek olvasása is itt van)
--    2. minden alap parser legyen token parser
--    3. az egész inputot topLevel-el olvassuk


-- Olvass be pozitív Int-ek hármasait listában!
-- Minden literál és szimbólum között lehet whitespace.
-- Példák : "[]", "[ (10, 20, 30) ]",  "[(0,1,2),(3,4,5)]"

intTriple :: Parser (Int, Int, Int)
intTriple = do
  char' '('
  n1 <- posInt'
  char' ','
  n2 <- posInt'
  char' ','
  n3 <- posInt'
  char' ')'
  pure (n1, n2, n3)

intTriple' :: Parser (Int, Int, Int)
intTriple' =
  (,,) <$> (char' '(' *> posInt')
       <*> (char' ',' *> posInt')
       <*> (char' ',' *> posInt'  <* char' ')')

intPair' :: Parser (Int, Int)
intPair' =
  (,) <$> (char' '(' *> posInt') <*> (char' ',' *> posInt' <* char' ')')

foo :: Parser Int
foo =
  (+) <$> (char' '(' *> posInt')
      <*> (char' ',' *> posInt' <* char' ')')

  -- (,)   :: a -> b -> (a, b)
  -- (,,)  :: a -> b -> c -> (a, b, c)
  -- (,,,) :: a -> b -> c -> d -> (a, b, c, d)

intTriples :: Parser [(Int, Int, Int)]
intTriples =
  topLevel (char' '[' *> sepBy intTriple (char' ',') <* char' ']')


-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- Írj egy parser-t ami, zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp = Lit Int | Plus Exp Exp deriving Show

pExp :: Parser Exp
pExp = undefined


-- Írj egy parser-t ami, zárójeleket, +-t, *-t és pozitív Int literálokat tartalmazó kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30 * 1
--           10 + 10 * 20
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))
-- A * operátor kössön erősebben a +-nál, és asszociáljon jobbra.
-- Minden literál és szimbólum között lehet whitespace.

data Exp2 = Lit2 Int | Plus2 Exp2 Exp2 | Mul Exp2 Exp2 deriving Show


-- bónusz: írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
