{-# language InstanceSigs #-}

import Control.Applicative hiding (optional)
import Data.Char

newtype Parser a = P { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \str -> case runParser p str of
    Nothing       -> Nothing
    Just (x,str') -> Just (f x, str')

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Just (x, str)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = P $ \str -> do
    (f, str')  <- runParser p str
    (x, str'') <- runParser q str'
    pure (f x, str'')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p k = P $ \str -> do
    (x, str') <- runParser p str
    runParser (k x) str'

combine :: Parser a -> Parser a -> Parser a
combine p q = P $ \str -> case runParser p str of
  Nothing  -> runParser q str
  Just res -> Just res

unit :: Parser a
unit = P (const Nothing)

instance Alternative Parser where
  empty :: Parser a
  empty = unit

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = combine

eof :: Parser ()
eof = P $ \str -> case str of
  "" -> Just ((), "")
  _  -> Nothing

char :: Char -> Parser Char
char c = P $ \str -> case str of
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing


--------------------------------------------------------------------------------

-- implementáld a következő parsereket!

-- kiolvas egy karaktert, ha a (Char -> Bool) függvény igaz a karakterre
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \cs -> case cs of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- akármilyen karaktert kiolvas
anyChar :: Parser Char
anyChar = satisfy (const True)

-- kiolvas nulla vagy több szóköz karaktert vagy newline-t
-- tipp: Data.Char.isSpace használható
ws :: Parser ()
ws = () <$ many (satisfy isSpace)

-- egy konkrét String-et olvas ki
string :: String -> Parser ()
string = mapM_ char


-- Írj egy parser-t, ami beolvas egy vagy több `a`-t `b`-vel
-- elválasztva, és visszaadja a beolvasott `a`-kat listában.
-- Pl. runParser (sepBy1 (char 'x') (char 'y')) "xyxyx" == "xxx"
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

-- Írj egy parser-t, ami mindig sikeres, és "Just a"-t ad vissza, ha
-- az input parser sikeres, egyébként Nothing-ot.
optional :: Parser a -> Parser (Maybe a)
optional pa = (Just <$> pa) <|> pure Nothing

-- Írj parser-t, ami úgy működik, mint a sepBy1, viszont végül opcionálisan
-- beolvas egy adott parsert.
sepEndBy1 :: Parser a -> Parser b -> Parser end -> Parser [a]
sepEndBy1 pa pb pend = sepBy1 pa pb <* optional pend

-- olvass be egy számjegyet
digit :: Parser Int
digit = read . (:[]) <$> satisfy isDigit

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

token :: Parser a -> Parser a
token pa = pa <* ws

-- token parserek: maguk után ws-t olvasnak
digit' = token digit
char' c = token (char c)
string' s = token (string s)


-- Írj egy parser-t, ami számjegyek vesszővel elválasztott listáit olvassa be.
digitCommaSepList :: Parser [Int]
digitCommaSepList = char' '[' *> sepBy digit' (char' ',') <* char' ']'


-- Példák:
--   [1, 4, 5, 1]
--   [  0  , 2  ]
--   []
--   [     ]
-- Használj ws-t a szóközök olvasásához.

-- Írj egy parser-t, ami helyes zárójelezéseket olvas.
-- Példák:
--   "()"
--   ""
--   "()()()"
--   "(())()"
--   "((()()))"
parenExpr :: Parser ()
parenExpr = some expr *> eof where
  expr = char '(' *> (() <$ many expr) <* char ')'


-- Írj egy parser-t, ami zárójelek + számliterálok + összeadás
-- kifejezéseket ismer fel. Whitespace beszúrható akárhova.
-- példák:

-- (12 + 0)
--  10 + 10
--  10 + 10 + 10 + 10

arithExpr :: Parser ()
arithExpr = ws *> addition <* eof where
  parens p = char' '(' *> p <* char' ')'
  literal  = token (() <$ some (satisfy isDigit))
  atom     = parens addition <|> literal
  addition = () <$ sepBy1 atom (char' '+')
