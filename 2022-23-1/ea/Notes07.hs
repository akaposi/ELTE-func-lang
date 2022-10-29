{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Data.Char  -- isDigit, isAlpha, digitToInt

-- Parser (Monád)
------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

--  input                    visszatérési érték    olvasás után maradék
-- String     ->    Maybe (  a                   , String)

-- emlékezzünk: State
--    s -> (a, s)

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing     -> Nothing
    Just (a, s) -> Just (f a, s)

instance Applicative Parser where
  pure :: a -> Parser a        -- nem olvasunk semmit, visszaadjuk "a"-t
  pure a = Parser $ \s -> Just (a, s)

  (<*>) = ap

instance Monad Parser where
  return = pure

  -- egymás után olvasunk, ha az első parser hibát dob, akkor
  -- a hibát propagáljuk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

  -- (lásd: lehet Parser-t közvetlenül definiálni, mint
  --  State + Maybe, monad transformer használatával)
  --  type Parser a = StateT String Maybe a

-- parser "kombinátor" stílus:
--   kis parserekből építünk nagy összetett parsereket

-- üres inputra ileeszkedik
eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

-- standard:
-- (<$) :: Functor f => a -> f b -> f a
-- (<$) a fb = fmap (\_ -> a) fb

-- példa: runParser (char 'a' >> char 'b' >> char 'c')
--         "abc"-t prbóbál olvasni

-- class Monoid m where
--   mempty :: m
--   (<>)   :: m -> m -> m

-- (Alternative: Monoid műveletek, funktorokhoz)
-- class Functor => Alternative f where
--   empty  :: f a
--   (<|>)  :: f a -> f a -> f a

instance Alternative Parser where

  -- rögtön hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- "choice":
  --   futtassuk az első parser-t, ha hibázik, akkor
  --   futtassuk a másodikat
  -- ("backtracking")
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- példa: runParser (char 'a' <|> char 'b')

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

-- definiáljuk ezeket újra:
many' :: Parser a -> Parser [a]
many' pa = some' pa <|> pure []

some' :: Parser a -> Parser [a]
some' pa = (:) <$> pa <*> many' pa

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

------------------------------------------------------------
-- reguláris kifejezéseket tudunk már írni

--   regex     Parser
----------------------
--     c       char c
--    p|q     p <|> q
--    pq      p >> q
--    p*      many_ p
--    p+      some_ p
--    ε       eof

p1 :: Parser ()
p1 = many_ (string "kutya" <|> string "macska")
-- runParser p1 "kutyamacska" == Just ((),"")
-- runParser p1 "kutyaxmacska" == Just ((),"xmacska")


------------------------------------------------------------
-- definiáljunk egy (Parser a)-t, amit regex-el nem lehet definiálni

nonregex :: Parser ()
nonregex = do
  as <- many (char 'a')
  replicateM_ (length as) (char 'b')

-- nonregex a következőt olvassa:
--  "", "ab", "aabb", "aaabbb", ...
-- (környezetfüggő parser)


-- strukturált adat olvasása
------------------------------------------------------------

-- implementáljuk egyszerű Haskell típusok olvasását String-ből

pBool :: Parser Bool
pBool = (True  <$ string "True")
    <|> (False <$ string "False")

-- alternatív def (monádikus)

pTrue' :: Parser Bool
pTrue' = do
  string "True"
  pure True

pFalse' :: Parser Bool
pFalse' = do
  string "False"
  pure False

pBool' :: Parser Bool
pBool' = pTrue' <|> pFalse'

pBool'' :: Parser Bool
pBool'' = do {string "True"; pure True}
      <|> do {string "False"; pure False}

-- (tekintsünk el a whitespace kezelésétől)

pPair :: Parser a -> Parser b -> Parser (a, b)
pPair pa pb = do
  char '('
  a <- pa
  char ','
  b <- pb
  char ')'
  pure (a, b)

-- Applicative standard operátorok:
-- (<*) :: Applicative f => f a -> f b -> f a
-- (<*) fa fb = (\a b -> a) <$> fa <*> fb

-- (*>) :: Applicative f => f a -> f b -> f a
-- (*>) fa fb = (\a b -> b) <$> fa <*> fb

-- használat:
--   n-darab művelet fut egymás után
--   amelyikre a csőr mutat, annak az értékét adjuk visszan
pPair' :: Parser a -> Parser b -> Parser (a, b)
pPair' pa pb =
  (,) <$> (char '(' *> pa <* char ',')
      <*> (pb <* char ')')

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pList :: Parser a -> Parser [a]
pList pa = do
  char '['
  as <- sepBy pa (char ',')
  char ']'
  pure as

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- egyensúlyozott zárójelezés:
-- jó:     (), ()(), (()),  ()(()),
-- rossz:  ((

pParens' :: Parser ()
pParens' = many_ (char '(' >> pParens' >> char ')')

-- probléma: általában nem akarjuk, hogy
-- maradék legyen az inputból
-- használjuk az eof-t:
pParens :: Parser ()
pParens = pParens' >> eof

------------------------------------------------------------

-- some_ (char 'a') >> char 'a'

-- Nem CFG, hanem PEG ("parsing expression grammar")
-- q1 = p1 <|> p2 <|> p3 <|> ....
-- q2 = p1 <|> p2 <|> p3

-- (regex: nem regex, hanem)
-- foo ::= a | b | c ....
-- bar ::= ....
