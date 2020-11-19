
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State


newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ p = () <$ many p
some_ p = () <$ some p

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = satisfy (\c -> c1 <= c && c <= c2)

letter :: Parser ()
letter = () <$ (charRange 'a' 'z' <|> charRange 'A' 'Z')

digit :: Parser Int
digit = digitToInt <$> charRange '0' '9'

posInt :: Parser Int
posInt = do
  ds <- some digit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- BEAD feladat
--------------------------------------------------------------------------------

{-

data Exp
  = Lit Bool       -- true, false
  | And Exp Exp    -- e1 /\ e2
  | Or  Exp Exp    -- e1 \/ e2
  deriving (Show, Eq)

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

{-
Precedencia:
  - Atomi: true, false, zárójelezés
  - /\   : jobbra asszociál
  - \/   : jobbra asszociál

(whitespace megendedett)
-}

-- 1. token parser
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws

-- 2. precedencia parserek

pAtom :: Parser Exp
pAtom =  (Lit True  <$ string' "true")
     <|> (Lit False <$ string' "false")
     <|> (char' '(' *> pOr <* char' ')')

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pAtom (string' "/\\")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "\\/")

-- top level
pExp :: Parser Exp
pExp = topLevel pOr

tests = [
    runParser pExp "true" == Just (Lit True,"")
  , runParser pExp "true /\\ false" == Just (And (Lit True) (Lit False),"")
  , runParser pExp "true /\\ true /\\ false" == Just (And (Lit True) (And (Lit True) (Lit False)),"")
  , runParser pExp "true /\\ true \\/ false" == Just (Or (And (Lit True) (Lit True)) (Lit False),"")
  , runParser pExp "true /\\ (true \\/ false)" == Just (And (Lit True) (Or (Lit True) (Lit False)),"")]

-}


-- Regex parsolás
--------------------------------------------------------------------------------



{-
Írj parser-t egyszerű reguláris kifejezések nyelvéhez! A bemenet egy String,
a kimenet egy regex szintaxisfája. Precedenciák csökkenő erő sorrendjében:

Karakterliterál: csak betű vagy szám karakter lehet (Data.Char: isAlphaNum)
  (egyértelmű, mi literál és mi spec. karakter)

  - atomi kifejezés (karakterliterál, karakter range, zárójelezett kifejezés)
  - Many           : nem asszociatív
  - Some           : nem asszociatív
  - Concat         : jobb asszociatív (egymás után írt regex-ek)
  - Choice         : jobb asszociatív

(Nincs whitespace!)

Példák helyes inputra:

  abc|efg
  kutya|macska
  (kutya)*|(macska)+
  ((foo)*)+
  [a..z][0..9]+
-}

data Regex =
    Lit Char             -- c
  | Range Char Char      -- [c1..c2]
  | Choice Regex Regex   -- e1|e2
  | Concat Regex Regex   -- e1e2
  | Many Regex           -- e*
  | Some Regex           -- e+
  deriving (Eq, Show)

pLit :: Parser Char
pLit = satisfy isAlphaNum

pAtom :: Parser Regex
pAtom = (Lit <$> pLit)
    <|> (Range <$> (char '[' *> pLit <* string "..") <*> (pLit <* char ']'))
    <|> (char '(' *> pChoice <* char ')')

pMany :: Parser Regex
pMany = do
  r <- pAtom
  (Many r <$ char '*') <|> pure r

  -- Applikatív-al
  -- (\r m -> maybe r (\_ -> Many r) m) <$> pAtom <*> ((Just <$> char '*') <|> pure Nothing)

pSome :: Parser Regex
pSome = do
  r <- pMany
  (Some r <$ char '+') <|> pure r

pConcat :: Parser Regex
pConcat =  foldr1 Concat <$> some pSome
      --   foldr1 Concat <$> sepBy1 pSome (pure ())    -- nincs szeparátor!

pChoice :: Parser Regex
pChoice = foldr1 Choice <$> sepBy1 pConcat (char '|')

pRegex :: Parser Regex
pRegex = pChoice <* eof

  -- - atomi kifejezés (karakterliterál, karakter range, zárójelezett kifejezés)
  -- - Many           : nem asszociatív
  -- - Some           : nem asszociatív
  -- - Concat         : jobb asszociatív (egymás után írt regex-ek)
  -- - Choice         : jobb asszociatív


{-
Értékeld ki a beolvasott Regex-et! Minden szintaktikus regex-ből csinálj egy tényleges Parser-t!
-}
evalRegex :: Regex -> Parser ()
evalRegex r = case r of
  Lit c        -> char c
  Range c1 c2  -> () <$ (satisfy $ \c -> c1 <= c && c <= c2)
  Choice r1 r2 -> evalRegex r1 <|> evalRegex r2
  Concat r1 r2 -> evalRegex r1 *> evalRegex r2
  Many r       -> many_ (evalRegex r)
  Some r       -> some_ (evalRegex r)

-- regex nélkül:   many (string "kutya" <|> string "macska")
-- regex-el:       regex "(kutya|macska)*"

-- ha a parsolás nem sikeres, akkor dobjunk error/undefined hibát
regex :: String -> Parser ()
regex str = case runParser pRegex str of
  Nothing     -> error "invalid regex"
  Just (r, _) -> evalRegex r


-- BEAD: operátor parser (nem asszociatív operátor is)
