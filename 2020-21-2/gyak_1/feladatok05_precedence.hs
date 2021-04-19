a
{-# LANGUAGE DeriveFunctor #-}

-- BEAD: egyszerű operátor szintaxis, infixLeft/infixRight/infixNonAssoc segítségével megoldható
--    (+ whitespace lehet benne)

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State
import Debug.Trace

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)

  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

-- üres input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- feltételnek megfelelő karakter
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- konkrét karakter
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- bármilyen karakter
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét string
string :: String -> Parser ()
string str = mapM_ char str

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- + Control.Applicative-ból importálva:
--  - many   : nulla vagy több érték olvasása
--  - some   : egy vagy több érték olvasása

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- Beolvasunk először egy b-t, utána 0 vagy több a-t, az eredményeket
-- balra asszociálva kombináljuk az adott függvénnyel.
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

debug :: String -> Parser ()
debug msg = Parser $ \s -> trace (msg ++ "  input: " ++ s) (Just ((), s))

-- token parserek/kombinátorok
--------------------------------------------------------------------------------

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

string' :: String -> Parser ()
string' str = string str <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- precedencia kombinátorok
--------------------------------------------------------------------------------

infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp
    [exp1, exp2] -> pure $ combine exp1 exp2
    _            -> empty


--------------------------------------------------------------------------------

-- Olvass be kiegyensúlyozott zárójelezéseket! Példák helyes inputra:
--  "()", "()()", "(())", "(())()"

p1Atom :: Parser ()
p1Atom = char '(' *> p1Many <* char ')'

p1Many :: Parser ()
p1Many = many_ p1Atom

p1Top :: Parser ()
p1Top = topLevel p1Many

-- Ugyanezt írjuk meg precendencia kombinátorral.
p1Atom' :: Parser ()
p1Atom' = char '(' *> p1Many' <* char ')'

p1Many' :: Parser ()
p1Many' = infixLeft p1Atom'
  (pure ())  -- nincs szeparátor
  const      -- const :: () -> () -> (), a visszatérési érték nem fontos
             --     (tehát bármely f :: () -> () -> () jó lenne)

p1Top' :: Parser ()
p1Top' = topLevel p1Many'


-- Írj parsert a következő nyelvre: Int literál lehet benne, ++ operátor, lista kifejezések,
-- zárójelek, és tetszőleges whitespace. A (++) jobbra asszociál.
--   Példák szintaktikailag helyes inputra:
-- "[3, 4, 5] ++ []", "[]",  "[[5, 6, 7], [7, 9]]", "[] ++ 4333 + []", "0 ++ [4]"
--
-- Megjegyzés: az input nyilván nem feltétlenül típushelyes (pl. 0 ++ [] nem az) a szokásos
-- Haskell típusozás szerint, mivel csak parser-t írtunk.

data Exp2 = Lit2 Int | List2 [Exp2] | Append2 Exp2 Exp2 deriving Show

p2Atom :: Parser Exp2
p2Atom = (char' '(' *> p2Append <* char' ')')
     <|> (char' '[' *> (List2 <$> sepBy p2Append (char' ',')) <* char' ']')
     <|> (Lit2 <$> posInt')

p2Append :: Parser Exp2
p2Append = infixRight p2Atom (string' "++") Append2

p2Top = topLevel p2Append


-- Írd meg ugyanezt monádikusan!
p2AtomM :: Parser Exp2
p2AtomM =
       do {char' '('; e <- p2Append; char' ')'; pure e}
  <|>  do {char' '['; es <- sepBy p2Append (char' ','); char' ']'; pure (List2 es)}
  <|>  do {n <- posInt'; pure (Lit2 n)}

p2AppendM :: Parser Exp2
p2AppendM = infixRight p2AtomM (string' "++") Append2

p2TopM = topLevel p2AppendM


-- Írj parsert a következő konstrukciókat tartalmazó nyelvre. Csökkenő erősségi sorrendben:
--
--   atom : True, False
--   not  : prefix
--   &&   : infix, jobb asszoc
--   ||   : infix, jobb asszoc
--   ==   : infix, nem asszoc
--
-- Plusz zárójelek, whitespace

-- Példák:
--  "True", "True && False || True", "True && (False || True)",
--  "not True && not False", "not (not (not False))", "not False == True && True"

data Exp3 = Lit3 Bool | And3 Exp3 Exp3 | Or3 Exp3 Exp3
          | Not3 Exp3 | Eq3 Exp3 Exp3 deriving Show


p3Atom :: Parser Exp3
p3Atom = (char' '(' *> p3Eq <* char' ')')
    <|>  (Lit3 True  <$ string' "True")
    <|>  (Lit3 False <$ string' "False")

p3Not :: Parser Exp3
p3Not = (Not3 <$> (string' "not" *> p3Atom)) -- van prefix operátor
    <|> p3Atom                               -- nincs (default eset)

p3And :: Parser Exp3
p3And = infixRight p3Not (string' "&&") And3

p3Or :: Parser Exp3
p3Or = infixRight p3And (string' "||") Or3

p3Eq :: Parser Exp3
p3Eq = infixNonAssoc p3Or (string' "==") Eq3

p3Top = topLevel p3Eq


-- Írj parser-t lambda kifejezésekhez! A változónevek nemüres betűsorozatok, a
-- lambda pedig "\", azaz backslash. Megjegyzés: ha Haskell String-be teszünk
-- backslash-t, azt duplán kell írni az escape karakterek miatt. Whitespace
-- lehetséges. Példák:
--   "\\x. x", "(\\x. x) (\\x. x)", "(\\x. \\y. \\z. x)"
--   "\\f. \\ x. f x x (f x)", "f a b c d e f"


data Tm = Var String | Lam String Tm | App Tm Tm deriving Show

p4Name :: Parser String
p4Name = some (satisfy isLetter) <* ws

p4Atom :: Parser Tm
p4Atom = (char' '(' *> p4Lam <*  char' ')')
     <|> (Var <$> p4Name)

-- a függvényalkalmazás *balra* asszociál, és nincs szeparátor!
p4App :: Parser Tm
p4App = infixLeft p4Atom (pure ()) App

-- a lambda kifejezés prefix, mivel jobbra láncolható.
p4Lam :: Parser Tm
p4Lam = (Lam <$> (char' '\\' *> p4Name <* char' '.') <*> p4Lam)
    <|> p4App

p4Top :: Parser Tm
p4Top = topLevel p4Lam
