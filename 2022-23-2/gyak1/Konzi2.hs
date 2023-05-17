module Konzi2 where

import Control.Applicative
import Control.Monad
import Data.Char
import Gy09 hiding (Exp(..), pAdd, pMul, pAtom, pPow)


{-
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

instance Applicative Parser where
  (<*>) = ap
  pure a = Parser $ \s -> Just (a, s)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
  (Parser p1) >>= f = Parser $ p1 >=> \(a, s') -> runParser (f a) s'


eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (x:xs) | p x -> Just (x, xs)
  _            -> Nothing


---------------------------------------


-- Parser: valami struktúrálatlan dolgot strúktúráltá alakít
-- A struktúrálatlan dolog: String
-- A struktúra: a mellékhatás


-- satisfy :: (Char -> Bool) -> Parser Char
-- Az a parser, ami lenyeli a következő szimbolúmot és
-- assertálja hogy a predikátum a teljesül a akraktere

-- satisfy (== 'a') -- predikátum = == 'a' => Csak 'a' karaktert for parseolni

-- Parserek szekvenciája
-- (>>) -- p1 >> p2 = p2 eredménye marad meg
-- (*>) -- p1 *> p2 = p2 eredménye marad meg
-- (<*) -- p1 <* p2 = p1 eredménye marad meg

-- (<|>) (alt) -- parser választás
-- Ugyanarra!! bemenetre lefuttat két parsert és az első sikereset adja vissza
-- Backtrack

-- satisfy (== 'a') <|> satisfy (== 'b')
-- Nem szekvencia

-- (<*>) (ap) -- kap egy függvényt parseoló parsert és egy értékparsert és a függvényt alkalmazza az eredményre
-- (<$>) (fmap) -- alkalmaz egy függvényt a parser eredményére
-- (>>=) (bind) -- Felhasználja egy parser eredményét és egy új parsert csinál belőle
-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b


-- (- 1)
-- negativeSign :: Parser (Int -> Int)
-- positiveSign :: Parser (Int -> Int)
-- ((negativeSign <|> positiveSign) <*> number)

-- <$ felülírás
negativeSign :: Parser (Int -> Int)
negativeSign = negate <$ satisfy (== '-')

positiveSign :: Parser (Int -> Int)
positiveSign = id <$ satisfy (== '+')

number :: Parser Int
number = digitToInt <$> satisfy isDigit

{-
(negativeSign <|> positiveSign) >>= \f -> f <$> number
-}

-- Parseoljunk egy számot
-- majd annyi + karakter mint ammenyi a szám értéke volt
-- number >>= \i -> replicateM i (satisfy == '+')

{-
p1 :: Parser String
p1 = do
        i <- number
        replicateM i (satisfy == '+')

-}

-- Írjunk do notációval egy parsert ami az alma szót be parseolja

almaParser :: Parser ()
almaParser = do
  satisfy (== 'a')
  satisfy (== 'l')
  satisfy (== 'm')
  () <$ satisfy (== 'a')


-- char c = (() <$ satisfy (== c))
-- traverse :: (a -> Parser b) -> [a] -> Parser [b]
-- string s = traverse_ (\c -> char c) s
-- mapM <=> mapM_

-- f => f_ ugyanaz az f csak eldobja az eredményt

-- many és a some (0 vagy több) (1 vagy több)

-- sepBy, sepBy1

-- optional

-- between

-- leftAssoc, nonAssoc, rightAssoc, chainl1, chainr1

-- 2 ^ 3 ^ 4
-- (2 ^ 3) ^ 4 = 8^4 = nagy szám < 1000
-- 2 ^ (3 ^ 4) = 2 ^ 81 = mégnagyobb szám >>> 1000
-- 1 + 2 * 3

{-

        +
     /    \
    1     *
        /   \
       2     3

-}

{-

   f
 /  \
p   f
   /  \
   p   f
      / \ ...
      p


 f
/ \
f  p
/ \
f  p
...

-}

-- (==)
-- 1 == 2 == 3
-- False == 3
-- 1 == False
-}


data Exp = Hash Exp Exp | Add Exp Exp | Mul Exp Exp | Pow Exp Exp | Lit Int
  deriving (Eq, Show)


-- ^
-- *
-- +


pAtom :: Parser Exp
pAtom = (Lit <$> integer) <|> between (char '(') pHash (char ')')

pPow :: Parser Exp
pPow = rightAssoc Pow pAtom (char '^')

pMul :: Parser Exp
pMul = leftAssoc Mul pPow (char '*')

pAdd :: Parser Exp
pAdd = leftAssoc Add pMul (char '+')

-- #

pHash :: Parser Exp
pHash = rightAssoc Hash pAdd (char '#')
