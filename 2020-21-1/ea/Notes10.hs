{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char

-- jövő hét: extra házi 2
-- azután  : extra házi 3
-- határidő: vizsgaidőszak első hét péntek

-- github: előző félév könyvtár: vizsga példák
--         függvények: listák, magasabbrendű fv, monád (Maybe/State/IO) (lista nem), foldable/traversable,
--                     rekurzív fa addatípus

--         "while" parser + interpreter kiegészítés extra feature-el
--         köv előadás: "while" parser + interpreter




-- Parser + source position + jobb hibaüzenetek + backtracking kontroll
--------------------------------------------------------------------------------

-- https://github.com/Geal/nom  alapján
-- lásd még: parsec, attoparsec, megaparsec (Haskell, kicsit több varázslat és kevesebb kontroll)

-- source position (sor, oszlop)
type Pos = (Int, Int)

data Result e a
  = Success a String Pos
  | Fail                   -- parser logika megvalósításához használjuk (Fail esetén próbálhatunk másik <|> ágat)
  | Error e Pos            -- mindeképpen szintaktikus hiba (nem lehet belőle backtrack-elni)
  deriving (Functor, Show)

-- "e" legyen a dobott hibák típusa
newtype Parser e a = Parser {runParser :: String -> Pos -> Result e a}
  deriving Functor

instance Applicative (Parser e) where
  pure = return
  (<*>) = ap

instance Monad (Parser e) where
  return a = Parser $ \s p -> Success a s p
  Parser f >>= g = Parser $ \s p -> case f s p of
    Success a s' p' -> runParser (g a) s' p'
    Fail            -> Fail
    Error e p       -> Error e p

instance Alternative (Parser e) where
  -- empty :: Parser e a
  empty = Parser $ \_ _ -> Fail

  -- (<|>) ::  Parser e a -> Parser e a -> Parser e a
  Parser f <|> Parser g = Parser $ \s p -> case f s p of
    Fail -> g s p
    res  -> res        -- (Success/Error)
                       -- Success: nem kell g-t próbálni
                       -- Error  : tovább propagálódik (nem lehet "elkapni")

eof :: Parser e ()
eof = Parser $ \s p -> case s of
  [] -> Success () [] p
  _  -> Fail

updPos :: Char -> Pos -> Pos
updPos '\n' (line, col) = (line + 1, 0)
updPos _    (line, col) = (line, col + 1)

satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = Parser $ \s p -> case s of
  c:cs | f c -> Success c cs (updPos c p)
  _ -> Fail

anyChar :: Parser e Char
anyChar = satisfy (\_ -> True)

char :: Char -> Parser e ()
char c = () <$ satisfy (==c)

string :: String -> Parser e ()
string str = mapM_ char str

sepBy1 :: Parser e a -> Parser e sep -> Parser e [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

sepBy :: Parser e a -> Parser e sep -> Parser e [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Alternative f => f a -> f ()
many_ p = () <$ many p

some_ :: Alternative f => f a -> f ()
some_ p = () <$ some p


-- extra lehetőségek
--------------------------------------------------------------------------------

-- "e" hiba dobása:
throw :: e -> Parser e a
throw e = Parser $ \s p -> Error e p

-- Fail és Error közötti konverzió:

-- Fail-ből Error-t csinálunk
cut :: Parser e a -> e -> Parser e a
cut pa e = pa <|> throw e
   -- ha pa Error-t dob, akkor ezt az Error-t visszük tovább
   -- mindig a legbelső Error kerül a kimenetbe
   --   (nom library: lehetőségünk van a belső és külső Error-okat kombinálni/összegezni)

-- Error-ból Fail-t csinálunk (kerülni érdemes! ("lookahead" parsing))
try :: Parser e a -> Parser e a
try (Parser f) = Parser $ \s p -> case f s p of
  Error _ _ -> Fail
  res       -> res

ws :: Parser e ()
ws = many_ (satisfy $ \c -> c == ' ' || c == '\n')

char' :: Char -> Parser e ()
char' c = char c <* ws

string' :: String -> Parser e ()
string' s = string s <* ws

topLevel :: Parser e a -> Parser e a
topLevel pa = ws *> pa <* eof

--------------------------------------------------------------------------------

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp deriving (Eq, Show)

type ParserS a = Parser String a

-- hol van olyan pont, ahol egy parsernek mindenképp sikeresnek kell lenni, egyébként
-- nem helyes az input?

-- sepBy1: minimum 1 dolgot kell olvasni, tehát itt van lehetőség Error-t dobni!
-- változat:
sepBy1Cut :: Parser e a -> Parser e sep -> e -> Parser e [a]
sepBy1Cut pa psep e = (:) <$> (pa `cut` e) <*> many (psep *> pa)

pIntLit' :: ParserS Int
pIntLit' = do
  ds <- some (digitToInt <$> satisfy isDigit) <* ws
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

goodChar :: Char -> Bool
goodChar c = elem c "+*()" || isDigit c

pAtom :: ParserS Exp
pAtom =
         (satisfy (not . goodChar) *> throw "lexical error")
     <|> (Lit <$> pIntLit')
     <|> (char' '(' *> pAdd <* (char' ')' `cut` "expected closing parenthesis"))

pMul :: ParserS Exp
pMul = foldr1 Mul <$> sepBy1Cut pAtom (char' '*') "expected an integer literal or parenthesized expression"

pAdd :: ParserS Exp
pAdd = foldr1 Add <$> sepBy1 pMul (char' '+')

pExp :: ParserS Exp
pExp = ws *> pAdd <* (eof `cut` "expected an integer literal or parenthesized expression")

runPExp :: String -> Result String Exp
runPExp s = runParser pExp s (0, 0)

-- házi feladat: szép error nyomtatás
-- prettyError <az egész source String> <üzenet> <pozíció>
prettyError :: String -> String -> Pos -> String
prettyError = undefined

-- példa try használatára:
-- új parser meglévő pAdd felhasználásával
-- lusta vagyok, nem akaron a cut-okat átírni a pAdd-ban
-- <|> nem fogja meg az error-t, használom a try-t
--
-- (try: kerülni érdemes)
-- (hatékony megoldás: try helyett konkrét "lookahead" parser-t írni)
pExp' :: ParserS ()
pExp' = (() <$ try pAdd) <|> some_ (satisfy isLower)


{-
-- példa: runPExp "(10 + 10 * 20"

parse error:0:13:
  |
0 |  (10 + 10 * 20
  |               ^
Expected closing parenthesis

-}
