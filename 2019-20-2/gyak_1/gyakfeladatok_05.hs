
import Control.Monad
import Control.Applicative
import Data.Char -- isSpace, isLetter, isDigit, isAlpha

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s ->
       fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- Olvas egy karaktert amire True-t ad egy függvény
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)   -- extra feltétel, f c == True
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

--------------------------------------------------------------------------------

-- Olvass pozitív Int literálokat. Ne használd a "read" függvént!
posInt :: Parser Int
posInt = undefined

-- A következő a "takeWhile" függvény parser megfelelője, addig olvas
-- egy String-et, amíg egy feltétel igaz a karakterekre.
-- Pl: takeWhileP isDigit kiolvassa az összes számjegyet az input elejéről.
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP = undefined

-- "manyUntil pa pb" addig olvas ismételten (akár nullaszor) pa-t, amíg pb-nincs
-- olvasva.
-- Például: manyUntil (satisfy (const True)) (char 'x') minden karaktert
-- kiolvas az első 'x'-ig, és az 'x'-et is kiolvassa.
manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil = undefined


-- Írj egy egyszerű csv (comma separated values) parsert!
-- A formátum a következő:
--    - az inputban nulla vagy több sor lehet
--    - minden sorban vesszővel elválasztva szerepelnek vagy számok vagy pedig
--    - latin betűt tartalmazó szavak
--    - whitespace nem engedett meg az új sorokon kívül
--    - olvassuk eof-al az input végét
--
-- A parser adja vissza az összes sor listáját, ahol (Either Int String) tárolja
-- a szavakat vagy számokat.
-- Példa helyes inputra:
{-
foo,bar,12,31
40,50,60,kutya,macska
-}

csv1 :: Parser [[Either Int String]]
csv1 = undefined


-- Írj parsert típusozatlan lambda kalkulushoz!
--   https://en.wikipedia.org/wiki/Lambda_calculus
-- A változónevek álljanak csak latin betűkből, a
-- lambda pedig legyen backslash ('\\').
-- Whitespace megengedett minden token körül.
-- Olvassunk eof-ot a parsolás végén.

data Term = Var String | App Term Term | Lam String Term
  deriving Show

parseLC :: Parser Term
parseLC = undefined
