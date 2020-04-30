
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



-- feladatot lehet csinálni
-- előadást nézzétek következő órára
--
------------------------------------------------------------

-- akármilyen karaktert kiolvas
anyChar :: Parser Char
anyChar =
  satisfy (\_ -> True)   -- akármilyen karakter
-- anyChar = Parser $ \s -> case s of
--   []   -> Nothing
--   c:cs -> Just (c, cs)   -- c: visszaadott Char, cs: input hátralevő része

-- kiolvas nulla vagy több szóköz karaktert vagy newline-t
-- tipp: Data.Char.isSpace használható
ws :: Parser ()
ws = () <$ many (satisfy isSpace)    -- konstans

   -- alternatív:
   -- do
   --   cs <- many (satisfy isSpace)
   --   pure ()


-- Írj egy parser-t, ami beolvas egy vagy több `a` parsert `b`-vel elválasztva,
-- és visszaadja a beolvasott `a`-kat listában.
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = do
  a  <- pa              -- olvasunk egy a-t
  as <- many $ do       -- 0-szor vagy többször: olvasunk b után a-t
          b <- pb
          a <- pa
          pure a        -- visszaadjuk az a-t
  pure (a:as)

  -- char 'a'
  -- char 'b'
  -- sepBy1 (char 'a') (char 'b')
  --    példák beolvasott inputokra: "a", "aba", "ababa", ...
  --    some/many használatával?   regexírás

  -- (*>) és (<*)   : egymás után két művelet végrehajtása
  -- (*>)           : második paraméter értékét adja vissza
  -- (<*)           : első paraméterét

  -- pl: getLine <* getLine    visszaadja az első beolvasott sort
  -- pl: getLine *> getLine    visszaadja az második beolvasott sort

sepBy1' :: Parser a -> Parser b -> Parser [a]
sepBy1' pa pb =
  (:) <$> pa <*> many (pb *> pa)         -- 2-paraméteres fmap


-- Írj egy parser-t, ami mindig sikeres, és "Just a"-t ad vissza, ha
-- az input parser sikeres, egyébként Nothing-ot.
optional :: Parser a -> Parser (Maybe a)
optional pa = (Just <$> pa) <|> pure Nothing    -- tömör verzió

optional' :: Parser a -> Parser (Maybe a)
optional' (Parser f) = Parser $ \s -> case f s of
  Nothing     -> Just (Nothing, s)        -- belső hibát átalakítja külső Maybe-re
  Just (a, s) -> Just (Just a , s)        --   alakítja

-- optional: regex-ként: ?

-- runParser (optional (char 'x') >> char 'y')


-- Írj parser-t, ami úgy működik, mint a sepBy1, viszont végül opcionálisan
-- beolvas egy adott parsert.
sepEndBy1 :: Parser a -> Parser b -> Parser end -> Parser [a]
sepEndBy1 pa pb pend = sepBy1 pa pb <* optional' pend

-- Olvass be egy számjegyet! Használd az `isDigit` függvényt
digit :: Parser Int
digit = do
  c <- satisfy isDigit
  case c of
    '0' -> pure 0
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    '6' -> pure 6
    '7' -> pure 7
    '8' -> pure 8
    '9' -> pure 9
    _   -> error "impossible"
  -- lehetne rövidebben is ASCII kóddal

-- Írj egy parser-t, ami beolvas nulla vagy több `a` parsert `b`-vel
-- elválasztva, és visszaadja a beolvasott `a`-kat listában.  Pl. runParser
-- (sepBy1 (char 'x') (char 'y')) "xyxyx" == "xxx"
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

-- Írj egy parser-t, ami számjegyek vesszővel elválasztott listáit olvassa be,
-- szóközök nélkül. Pl: [0,1,2,3]
digitCommaSepList :: Parser [Int]
digitCommaSepList = do
  char '['
  ns <- sepBy digit (char ',')
  char ']'
  pure ns

digitCommaSepList' :: Parser [Int]
digitCommaSepList' =
  char '[' *> sepBy digit (char ',') <* char ']'


-- Írd meg az előző parsert, viszont engedd meg mindenhol szóköz beszúrását!
-- Pl. [ 1, 2   , 5 ] elfogadott.

-- kézzel beszúrjuk a ws-eket
digitCommaSepListSpaces :: Parser [Int]
digitCommaSepListSpaces =
 ws *> char '[' *> ws *> sepBy (digit <* ws) (char ',' *> ws) <* char ']' <* ws

-- ELŐADÁS: minden alap parser függvény maga után ws-t olvas
char' c    = char c <* ws
digit'     = digit <* ws
anyChar'   = anyChar <* ws
satisfy' f = satisfy f <* ws

topParser :: Parser a -> Parser a
topParser pa = ws *> pa <* eof

-- végső parser-nél: kezdő ws-t be kell olvasni (minden más ws már automatikusan kezelve van)
digitCommaSepListSpaces' :: Parser [Int]
digitCommaSepListSpaces' =
  topParser (
     char' '[' *> sepBy digit' (char' ',') <* char' ']')

-- előadás: +, *, ^, számliterál, zárójel olvasása  "10 * 20 * (32 + 43^2)"
-- runParser pExp "10 + 20 + 30" == Just (60, "")

-- atom :: Parser Int
-- atom = digit' <|> (char' '(' *> sumExp <* char' ')')

-- sumExp :: Parser Int
-- sumExp = sum <$> sepBy1 atom (char' '+')

-- pExp :: Parser Int
-- pExp = topParser sumExp


data Exp = Add Exp Exp | Digit Int
  deriving Show

atom :: Parser Exp
atom =  (Digit <$> digit')
    <|> (char' '(' *> sumExp <* char' ')')

sumExp :: Parser Exp
sumExp = foldl1 Add <$> sepBy1 atom (char' '+')

pExp :: Parser Exp
pExp = topParser sumExp
