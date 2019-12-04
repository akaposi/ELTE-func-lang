-- Parser monád (folytatás)

{-# language InstanceSigs #-}

import Control.Applicative
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

instance Alternative Parser where
  empty :: Parser a
  empty = P (const Nothing)

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

-- kiolvas nulla vagy több szóköz karaktert vagy newline-t
-- tipp: Data.Char.isSpace használható
ws :: Parser ()
ws = undefined

-- egy konkrét String-et olvas ki
string :: String -> Parser ()
string = undefined

-- akármilyen karaktert kiolvas
anyChar :: Parser Char
anyChar = undefined

-- kiolvas egy karaktert, ha a (Char -> Bool) függvény igaz a karakterre
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = undefined

-- Írj egy parser-t, ami beolvas egy vagy több `a`-t `b`-vel
-- elválasztva, és visszaadja a beolvasott `a`-kat listában.
-- Pl. runParser (sepBy1 (char 'x') (char 'y')) "xyxyx" == "xxx"
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

-- olvass be egy számjegyet
digit :: Parser Int
digit = undefined

-- Írj egy parser-t, ami számjegyek vesszővel elválasztott listáit olvassa be.
digitCommaSepList :: Parser [Int]
digitCommaSepList = undefined
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
parenExpr = undefined

-- Írj egy parser-t, ami zárójelek + számliterálok + összeadás
-- kifejezéseket ismer fel. Whitespace beszúrható akárhova.
-- példák:

-- (12 + 0)
--  10 + 10
--  10 + 10 + 10 + 10

arithExpr :: Parser ()
arithExpr = undefined


-- Írj egy parser-t, ami típusozatlan lambda kifejezéseket olvas be!

data Tm =
    Lam String Tm  -- lam x. t
  | App Tm Tm      -- t u
  | Var String     -- x

-- változónevek legyenek nemüres, csak betűből álló String-ek
-- bármilyen önmagában helyes kifejezés zárójelbe tehető
-- whitespace beszúrható akárhova

-- helyes példák:
--   lam x. lam y. x
--   (lam x. x) (lam x. x)
--   (lam x. x) ((lam x. x))
--   lam f. lam x. f (f (f x))
--   lam f. lam g. lam x. f (g x)
--   (lam f. (lam g. lam x. f (g x)))

lamExpr :: Parser Tm
lamExpr = undefined
