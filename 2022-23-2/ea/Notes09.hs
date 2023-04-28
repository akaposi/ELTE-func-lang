
import Control.Monad
import Control.Applicative
import Data.Char  (isSpace)

-- Parser folyt
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where

  -- return: nincs mellékhatás
  return :: a -> Parser a
  return = pure

  -- egymást után két parser-t végrehajtunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser f) g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

-- parser, ami üres inputot fogadja csak el
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- sikeres: az input első karakterére igaz egy feltétel,
-- kiolvassuk és visszaadjuk
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- konkrét karaktert olvasunk
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

instance Alternative Parser where
  -- rögtön hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- ha pa hibázik, futtassuk pb-t
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String-et olvassunk
string :: String -> Parser ()
string s = mapM_ char s

-- standard függvények: Control.Applicative-ból
-- many, some

-- futtassunk egy parsert annyiszor, ahányszor lehet,
-- adjuk vissza az összes eredményt listában. (0-szor vagy többször futtatunk valamit)
-- many :: Parser a -> Parser [a]

-- (1-szer vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]

-- definiáljuk újra. Tipp: standard definíció: kölcsönösen rekurzív.
many' :: Parser a -> Parser [a]
many' pa = some' pa <|> pure []
     -- 1-szer vagy többször <|> 0-szor

some' :: Parser a -> Parser [a]
some' pa = (:) <$> pa <*> many' pa   -- bináris fmap

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

-- reguláris kifejezések
------------------------------------------------------------

{-
    Parser ()          regex

     char c             c
    p₁ >> p₁           p₁p₂
    p₁ <|> p₂          p₁|p₂
      eof               $
    many_ p             p*
    some_ p             p+
                       ?p
-}

-- futtassunk valamit 0-szor vagy 1-szer
optional :: Parser a -> Parser (Maybe a)
optional pa = (Just <$> pa) <|> pure Nothing

-- Char-t olvas két adott Char között
charRange :: Char -> Char -> Parser Char
charRange c1 c2 = satisfy (\c -> c1 <= c && c <= c2)

charRange_ :: Char -> Char -> Parser ()
charRange_ c1 c2 = () <$ charRange c1 c2

digitChar :: Parser Char
digitChar = charRange '0' '9'

-- feladat: (kutya|macska)*[0-9]+
f1 :: Parser ()
f1 = many_ (string "kutya" <|> string "macska") >> some_ digitChar

-- Írjunk egy (Parser ()), ami nem regex:

-- a^n b^n
nonRegex :: Parser ()
nonRegex = do
  str <- many (char 'a')
  replicateM_ (length str) (char 'b')
  eof

-- runParser nonRegex "aaaabbbb" == Just ((),"")
-- runParser nonRegex "aaaabbbbb" == Nothing

-- Parserek:
--   Applicative: kontextusfüggetlen nyelv
--   Monad      : kontextusfüggő

endsOnAbc :: Parser ()
endsOnAbc = Parser $ \s -> case reverse s of
  'c':'b':'a':_ -> Just ((), "")
  _             -> Nothing

-- pontosítva:
-- Applicative + rekurzió: PEG parser ("parsing expression grammar")
-- Itteni library algoritmikusan: ún. "recursive descent"
--                              (clang, rust, gcc parserek: szintén recursive descent)

-- Strukturált adat olvasás
--------------------------------------------------------------------------------


-- Haskell szintaxisú adat olvasása

posInt :: Parser Int   -- hf csalás nélkül
posInt = read <$> some digitChar

-- 0 vagy több érték szeparált olvasása
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = ((:) <$> pa <*> many (psep >> pa)) <|> pure []

-- goIntList :: Parser [Int]
-- goIntList =  ((:) <$> posInt <*> many (char ',' >> posInt)) <|> pure []

parseIntList :: Parser [Int]
parseIntList = do
  char '['
  ns <- sepBy posInt (char ',')
  char ']'
  pure ns

parseListPair :: Parser ([Int], [Int])
parseListPair = do
  char '('
  ns1 <- parseIntList
  char ','
  ns2 <- parseIntList
  char ')'
  pure (ns1, ns2)

-- Applicative stílusban Parser kombinátorok:

parseList :: Parser a -> Parser [a]                -- "parser kombinátor" library
parseList pa = char '[' *> sepBy pa (char ',') <* char ']'

-- standard Applicative operátorok:

-- egymás után végrehajtás, de az első művelet értékét adjuk vissza
--    (<*) :: Applicative f => f a -> f b -> f a

-- egymás utáni végrehajtás ((>>) szinonímája)
--    (*>) :: Applicative f => f a -> f b -> f b

parsePair :: Parser a -> Parser b -> Parser (a, b)
parsePair pa pb =
  (,) <$> (char '(' *> pa) <*> (char ',' *> pb <* char ')')

-- Applicative idióma:
--    p1 *> p2 *> p3 <* p4 ..... <* p20
-- Egymás utáni végrehajtás, a csőrök rámutatnak arra a műveletre, aminek
-- az értékét vissza szeretnénk adni

myParser :: Parser [([Int], [Int])]
myParser = parseList (parsePair (parseList posInt) (parseList posInt))


-- Whitespace-t hogyan kezeljük?
------------------------------------------------------------

-- parser, ami megeszi a whitespace-eket
ws :: Parser ()
ws = many_ (satisfy isSpace)

-- buta megoldás: beszórjuk a ws-t a korábbi definíciókba

-- standard megoldás: token parsolási konvenciót betartjuk:
--    1. definiáljuk a ws-t
--    2. definiáljuk az összes primitív parser-t
--           (olyan művelet, ami közvetlenül karakter fogyaszt az inputból)
--    3. minden primitív parser maga után a ws-t elfogyasztja
--    4. a teljes parser legelején +1 ws-t hívunk
--    bónusz: a teljes parser legvégén pedig eof-ot hívunk

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- vesszős parserek kezelik a ws-t
char' :: Char -> Parser ()
char' c = char c <* ws

posInt' :: Parser Int
posInt' = posInt <* ws

parsePair' :: Parser a -> Parser b -> Parser (a, b)
parsePair' pa pb =
  (,) <$> (char' '(' *> pa) <*> (char' ',' *> pb <* char' ')')

parseList' :: Parser a -> Parser [a]
parseList' pa = char' '[' *> sepBy pa (char' ',') <* char' ']'

myParser' :: Parser [([Int], [Int])]
myParser' =
  topLevel (parseList' (parsePair' (parseList' posInt') (parseList' posInt')))

--------------------------------------------------------------------------------
-- Következő:
--     - operátorok, precedencia
--     - kulcsszavak és azonosítók
