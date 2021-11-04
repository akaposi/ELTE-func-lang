{-# LANGUAGE InstanceSigs #-}

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

-- Parser a : String-ből "a" típusú értéket próbál olvasni
newtype Parser a =
  Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing      -> Nothing
    Just (a, s') -> Just (f a, s')

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

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét String-et próbál olvasni
string :: String -> Parser ()
string = traverse_ char

-- many :: Parser a -> Parser [a]        -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]        -- 1-szer vagy többször olvasás

------------------------------------------------------------

-- Monadikus / Applicative parserek
-- séma arra, hogy operátorokat / nyelvi kifejezéseket hogyan lehet parsolni

------------------------------------------------------------

-- Strukturált megoldások a következőkre:
--   - Whitespace kezelése
--   - Operátorok és kötési erősségük
--   - Azonosítók
--   - Kulcsszavak kezelése

-- példa whitespace kezelésére:

-- p1: csv-jellegű parser
--     - számjegyek veszzővel elválasztott listája
--     - minden szimbólum között lehet whitespace
--     - pl:   [ 2, 4, 5, 6 ]        [      1    ,     5  ]

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

-- whitespace elfogyasztása
ws :: Parser ()
ws = () <$ many (char ' ')

-- (Standard függvények parser library-kben)

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

  -- f <$> ma
  -- f <$> ma <*> mb
  -- f <$> ma <*> mc <*> md
  -- ...

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

p1 :: Parser [Int]
p1 = do
  ws
  char '['
  ws
  ns <- sepBy (digit <* ws) (char ',' *> ws)
  char ']'
  ws
  pure ns

-- Applicative verzió:
p1' :: Parser [Int]
p1' =
  ws *> char '[' *> ws *> sepBy (digit <* ws) (char ',' *> ws) <* char ']' <* ws

-- ws-ek kezelése:
--   primitív parserek: "token" parser-ek legyenek
--     token parser: maga *után* ws-t olvas

--   ha minden alapvető parser token parser:
--      - minden ws-t kezelünk kivéve az input legelején
--      - "top-level" parser: ws olvasása az input elejéről

-- kezeli a kezdő ws-t + illeszkedik az input végére
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- konvenció: primitív token parserek legyen ' nevűek:

char' :: Char -> Parser ()
char' c = char c <* ws

digit' :: Parser Int
digit' = digit <* ws

string'  :: String -> Parser ()
string' str = string str <* ws

p1'' :: Parser [Int]
p1'' = char' '[' *> sepBy digit' (char' ',') <* char' ']'

p1Top :: Parser [Int]
p1Top = topLevel p1''

-- library-k : parsec, attoparsec, megaparsec


-- operátor parsolás
------------------------------------------------------------

-- szám literál, +, *, zárójelezés

-- konstrukciók és erősségük (csökkenő sorrendben)

--     literál, zárójelezés       ("zárt", "atomi" elemek, "végtelen" erősség,
--                                 olvasás nem függ a bal/jobb környezettől)
--     *: bináris, jobbra asszociál
--     +: bináris, jobbra asszociál


-- Megoldás menete (rekurzív precedencia-parsolás):
--    - Minden kötési erősséghez definiálunk egy függvényt
--      - adott erősségű konstrukciót olvassák, 1-el erősebb parser-t hívják
--      - zárójelezés/zárt konstrukció: a leggyengébb parser-t hívjuk


-- output: kifejezésfa (AST, abstract syntax tree)
data Exp = IntLit Int | Add Exp Exp | Mul Exp Exp
  deriving (Eq, Show)

e1 :: Exp
e1 = Add (IntLit 100) (IntLit 200)  -- 100 + 200

e2 :: Exp
e2 = Mul (Add (IntLit 100) (IntLit 200)) (IntLit 5)     -- (100 + 200) * 5

-- primitív (token) parserek
------------------------------------------------------------

intLit' :: Parser Int            -- read: String -> Int (kivételt dobhat)
intLit' = read <$> (some (satisfy isDigit) <* ws)

------------------------------------------------------------

pAtom :: Parser Exp
pAtom = debug "atom" (
       (IntLit <$> debug "lit" intLit')
   <|> (char' '(' *> pAdd <* char' ')'))

-- 1 vagy több atomi kifejezés *-val elválasztva
pMul :: Parser Exp
pMul = debug "mul" (foldr1 Mul <$> sepBy1 pAtom (char' '*'))
  -- do exps <- sepBy1 pAtom (char' '*')
  --    pure (foldr1 Mul exps)

-- 1 vagy több mul kifejezés +-val elválasztva
pAdd :: Parser Exp
pAdd = debug "add" (foldr1 Add <$> sepBy1 pMul (char' '+'))

------------------------------------------------------------

pExp :: Parser Exp
pExp = topLevel pAdd

-- debug parser
------------------------------------------------------------

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)


-- példák:
------------------------------------------------------------
-- 10 + 20   olvasása

-- runParser pExp "10 + 10"
-- add  : 10 + 10
-- mul  : 10 + 10
-- atom : 10 + 10
-- mul  : 10
-- atom : 10
-- Just (Add (IntLit 10) (IntLit 10),"")

-- extra házi: még több/szebb debug outputot adni a parser-hez
