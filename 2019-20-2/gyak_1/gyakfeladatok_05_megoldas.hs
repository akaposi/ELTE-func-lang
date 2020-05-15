

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

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

--------------------------------------------------------------------------------

posInt :: Parser Int
posInt = do
  ds <- some (satisfy isDigit)
  pure $ fst $ foldr (\c (acc, i) -> (acc + digitToInt c * i, i*10)) (0, 1) ds

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP f = many (satisfy f)

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil pa pb =  (pb >> pure [])
               <|> ((:) <$> pa <*> manyUntil pa pb)


-- csv
------------------------------------------------------------

isLatinChar :: Char -> Bool
isLatinChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

numberOrWord :: Parser (Either Int String)
numberOrWord =
      (Left <$> posInt)
  <|> (Right <$> some (satisfy isLatinChar))

line :: Parser [Either Int String]
line = sepBy numberOrWord (char ',')

csv1 :: Parser [[Either Int String]]
csv1 = sepBy line (char '\n') <* eof


-- lambda kalkulus
------------------------------------------------------------

data Term = Var String | App Term Term | Lam String Term
  deriving Show

ws :: Parser ()
ws = () <$ many (satisfy $ \c -> c == ' ' || c == '\n')

char' :: Char -> Parser ()
char' c = char c <* ws

pName :: Parser String
pName = some (satisfy isLatinChar) <* ws

pAtom :: Parser Term
pAtom = (Var <$> pName)
    <|> (char' '(' *> pTerm <* char' ')')

pApps :: Parser Term
pApps = foldl1 App <$> some pAtom

pTerm :: Parser Term
pTerm = (Lam <$> (char' '\\' *> pName <* char' '.') <*> pTerm)
    <|> pApps

parseLC :: Parser Term
parseLC = ws *> pTerm <* eof
