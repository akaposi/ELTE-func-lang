
import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace (trace, traceShow, traceM, traceShowM)

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

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

posInt :: Parser Int
posInt = read <$> some (satisfy isDigit)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s ->
  trace (msg ++ " : " ++ s) (runParser pa s)

-- Precedencia segédfüggvények
------------------------------------------------------------

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

-- nem asszociatív:
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

-- nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa

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
--    lásd "topLevel" alább

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- vesszős parserek kezelik a ws-t
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

posInt' :: Parser Int
posInt' = posInt <* ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

-- Parser folyt.
--------------------------------------------------------------------------------

{-
-- Precedencia, kulcsszavak, azonosítók
-- Interpreter

data Exp = Add Exp Exp | Mul Exp Exp | Lit Int
  deriving (Eq, Show)

-- 10 + 10 * 20        Add (Lit 10) (Mul (Lit 10) (Lit 20))

e1 = Add (Lit 10) (Mul (Lit 10) (Lit 20))

-- Olvassunk Exp-et String-ből.
-- Specifikáció: listázzunk mindent kötési erősség csökkenő sorrendjében
--
--  "atom" (végtelen erősség) (nem függ a bal/jobb környezettől)

--    - atom:      számliterál, zárójelezett kifejezés
--    - szorzás:   jobbra asszociál, bináris
--    - összeadás: jobbra asszoc, bináris

-- megoldás: minden erősségi szinthez írunk egy függvényt
--           "gyengébb függvény hívja az erősebbeket"

atom :: Parser Exp
atom =
  debug "atom"
  ((Lit <$> posInt') <|> (char' '(' *> add <* char' ')'))
-- példák:  10, 20, (10+20), (((10)))

-- 1 vagy több *-al szeparált atom
-- példák: 10 * 20    (20+10) * 20 * 30     ((20 + 30))

mul :: Parser Exp
mul =
  debug "mul" $
  foldr1 Mul <$> sepBy1 atom (char' '*')

-- 1 vagy több +-al szeparált mul kifejezés
add :: Parser Exp
add =
  debug "add" $
  foldr1 Add <$> sepBy1 mul (char' '+')

parseExp :: Parser Exp
parseExp = topLevel add

-- algoritmus: "recursive precedence parsing"
--  alternatív szervezése ugyanennek: "Pratt parsing"
--   G<F<int >>

foo :: (Int -> Int -> Int) -> Int
foo f =
  20 ** 20 ** 20 ** 30 where
  (**) = f; infixr 5 **
-}

------------------------------------------------------------

{-
data Exp = Add Exp Exp | Mul Exp Exp | Lit Int
  deriving (Eq, Show)

atom     = ((Lit <$> posInt') <|> (char' '(' *> add <* char' ')'))
mul      = rightAssoc Mul atom (char' '*')
add      = rightAssoc Add mul (char' '+')
parseExp = topLevel add

-- mkOperatorParser :: PrecTable Exp -> Parser Exp
--
-}

-- Azonosító vs kulcsszó
------------------------------------------------------------

data Tm = Var String | BoolLit Bool | And Tm Tm | Not Tm deriving Show

-- not x         Not (Var "x")
-- x && y && z   And (Var "x") (And (Var "y") (Var "z"))

--    változó: nemüres betűből álló string, ami *nem* "not"
--
--    atom: változó, zárójelett kifejezés
--    not:  prefix operátor
--    &&    jobbra asszoc

-- Változók és kulcsszavak egyértelműsítése:
--   1. definiáljuk a kulcsszavak listáját
--   2. a változó nem lehet kulcsszó
--   3. a kulcsszó nem folytatódhat valid változó karakterrel

keywords :: [String]
keywords = ["not", "true", "false"]

-- olvassunk egy konkrét kulcsszót
keyword :: String -> Parser ()
keyword s = do
  string s
  m <- optional (satisfy isLetter)
  case m of
    Just _ -> empty
    _      -> ws

ident :: Parser String
ident = do
  s <- some (satisfy isLetter) <* ws
  if elem s keywords
    then empty
    else pure s

atom :: Parser Tm
atom =(Var <$> ident)
  <|> (char' '(' *> pAnd <* char' ')')
  <|> (BoolLit True  <$ keyword "true")
  <|> (BoolLit False <$ keyword "false")

pNot :: Parser Tm
pNot = prefix Not atom (keyword "not")

pAnd :: Parser Tm
pAnd = rightAssoc And pNot (string' "&&")

tm = topLevel pAnd

------------------------------------------------------------
