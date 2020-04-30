
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

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

------------------------------------------------------------

-- akármilyen karaktert kiolvas
anyChar :: Parser Char
anyChar = undefined

-- kiolvas nulla vagy több szóköz karaktert vagy newline-t
-- tipp: Data.Char.isSpace használható
ws :: Parser ()
ws = undefined

-- Írj egy parser-t, ami beolvas egy vagy több `a` parsert `b`-vel elválasztva,
-- és visszaadja a beolvasott `a`-kat listában.  Pl. runParser (sepBy1 (char
-- 'x') (char 'y')) "xyxyx" == "xxx"
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 = undefined

-- Írj egy parser-t, ami mindig sikeres, és "Just a"-t ad vissza, ha
-- az input parser sikeres, egyébként Nothing-ot.
optional :: Parser a -> Parser (Maybe a)
optional = undefined

-- Írj parser-t, ami úgy működik, mint a sepBy1, viszont végül opcionálisan
-- beolvas egy adott parsert.
sepEndBy1 :: Parser a -> Parser b -> Parser end -> Parser [a]
sepEndBy1 = undefined

-- Olvass be egy számjegyet! Használd az `isDigit` függvényt
digit :: Parser Int
digit = undefined

-- Írj egy parser-t, ami beolvas nulla vagy több `a` parsert `b`-vel
-- elválasztva, és visszaadja a beolvasott `a`-kat listában.  Pl. runParser
-- (sepBy1 (char 'x') (char 'y')) "xyxyx" == "xxx"
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

-- Írj egy parser-t, ami számjegyek vesszővel elválasztott listáit olvassa be,
-- szóközök nélkül. Pl: [0,1,2,3]
digitCommaSepList :: Parser [Int]
digitCommaSepList = undefined

-- Írjd meg az előző parsert, viszont engedd meg mindenhol szóköz beszúrását!
-- Pl. [ 1, 2   , 5 ] elfogadott.
digitCommaSepListSpaces :: Parser [Int]
digitCommaSepListSpaces = undefined
