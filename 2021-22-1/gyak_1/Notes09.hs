
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable,
    DeriveTraversable #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

import Control.Monad.State  -- (miért nem ezt írjuk?)
                            -- (az itteni definíció bonyolultabb,
                            --  a típushibák nem azok, mint amit mi néztünk)

-- következő Canvas feladat:
--    - Haskell szintaxisban adat olvasása (listák, tuple-ök, Either/Maybe egymásba ágyazva)
--    - token parserekkel (ws olvasásával)

--------------------------------------------------------------------------------

-- <[a-z]+>(,<[a-z]+>)*

lowercase = satisfy_ (\c -> 'a' >= c && c <= 'z')

canvas :: Parser ()
canvas = do
  char '<'
  some_ lowercase
  char '>'
  many_ $ do
    string ",<"
    some_ lowercase
    char '>'


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
ws = many_ (char ' ')

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit


-- FELADATOK (whitespace/token parsing)
--------------------------------------------------------------------------------

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- 1. alap/primitív parser-ből definiálunk "token parser" verziót
--    token parser: maga után whitespace-t kiolvas

-- 2. tényleges (top-level) parser legyen "topLevel ..." formájú

intLit' :: Parser Int
intLit' = do
  isNeg <- (True <$ char '-') <|> pure False
  n <- read <$> some (satisfy isDigit)
  ws
  if isNeg then pure (n * (-1))
           else pure n

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Whitespace használata megengedett bárhol!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
p1 :: Parser [Int]
p1 = topLevel $ do
  char' '['
  ns <- sepBy intLit' (char' ',')
  char' ']'
  pure ns

-- p1 = char' '[' *> sepBy intLit' (char' ',') <* char' ']'

listOf :: Parser a -> Parser [a]
listOf pelem = do
  char' '['
  ns <- sepBy pelem (char' ',')
  char' ']'
  pure ns

intPair :: Parser (Int, Int)
intPair = do
  char' '('
  n1 <- intLit'
  char' ','
  n2 <- intLit'
  char' ')'
  pure (n1, n2)

-- intPair = (,) <$> (char' '(' *> intLit')
--               <*> (char' ',' *> intLit' <* char' ')')

-- Írj egy parsert, ami beolvas [(Int, Int)] típusú értékeket, Haskell
-- szintaxist követve. Whitespace mindenhol előfordulhat.
p2 :: Parser [(Int, Int)]
p2 = topLevel (listOf intPair)

intMaybeInt :: Parser (Int, Maybe Int)
intMaybeInt = do
  char' '('
  n1 <- intLit'
  char' ','
  n2 <- (Nothing <$ string' "Nothing")
    <|> (do string' "Just"
            n <- intLit'
            pure (Just n))
  char' ')'
  pure (n1, n2)

-- -- Applicative verzió (már kicsit túl nagy ehhez a stílushoz)
-- intMaybeInt :: Parser (Int, Maybe Int)
-- intMaybeInt =
--   (,) <$> (char' '(' *> intLit' <* char' ',')
--       <*> ((   (Nothing <$ string' "Nothing")
--            <|> (Just <$> (string' "Just" *> intLit'))) <* char' ')')

-- Írj egy parsert, ami beolvas [(Int, Maybe Int)] típusú értékeket, Haskell szintaxist
-- követve. Whitespace mindenhol előfordulhat.
p3 :: Parser [(Int, Maybe Int)]
p3 = topLevel (listOf intMaybeInt)



-- FELADATOK (operátor parsing)
--------------------------------------------------------------------------------


-- infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
-- infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

-- infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
-- infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

-- infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
-- infixNonAssoc pa psep combine = do
--   exps <- sepBy1 pa psep
--   case exps of
--     [exp]        -> pure exp                  -- 1 db pa kifejezés
--     [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
--     _            -> empty


-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas!
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp = Lit Int | Plus Exp Exp deriving (Eq, Show)

pExp :: Parser Exp
pExp = debug "pExp" (topLevel plusExp)


-- 0. definiáljuk a ws-t és a token parsereket ("lexikális")
-- 1. felírjuk kötési erősség szerinti sorrendben a nyelvi elemeket
--    - minden erősséghez írunk egy függvényt
--    - gyengébb függvény hívja az 1-el erősebbet
--         (adott fv parsolja az adott konstrukciót + bármilyen erősebb konstrukciót)
--    - zárójel belsejében hívjuk a leggyengébb függvényt


-- zárt kifejezések (két oldalról kulcsszó/szimbólum lezárja őket)
--  (zárójelezett Exp + literál)
atom :: Parser Exp
atom = debug "atom" (
       (Lit <$> intLit')
   <|> (char' '(' *> plusExp <* char' ')'))


-- összeadás kifejezés
-- 1 vagy több atom, +-al elválasztva
plusExp :: Parser Exp
plusExp = debug "plusExp" (foldr1 Plus <$> sepBy1 atom (char' '+'))

-- Írj egy parser-t, ami zárójeleket, ++-t és String literálokat olvas!
-- a ++ asszociáljon jobbra, a String literál pedig legyen testszőleseg
-- nem '"' karatkerek sorozata két '"' karakter között. Whitespace mindenhol
-- megengedett, viszont, ügyeljünk arra, hogy a whitespace a String literál
-- belsejében releváns!
--   példák: "foo" ++ "bar"
--           ""
--           ("kutya" ++ "macska") ++ "béka"

data Exp2 = StrLit String | Append Exp2 Exp2 deriving (Eq, Show)


-- bónusz: Írj egy parsert, ami pontosan a kiegyensúlyozott
-- zárójel-sorozatokat ismeri fel!  Helyes példák: "", "()", "()()", "(())()",
-- "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- bónusz: írj parser-t típusozatlan lambda kalkulushoz!
-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)
data Tm = Var String | App Tm Tm | Lam String Tm deriving (Eq, Show)

pTm :: Parser Tm
pTm = undefined
