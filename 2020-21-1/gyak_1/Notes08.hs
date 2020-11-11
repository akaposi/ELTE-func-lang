{-# LANGUAGE DeriveFunctor, RankNTypes, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Data.Char

import Control.Monad.State


-- BEAD megoldás
--------------------------------------------------------------------------------

data Tree = Node Tree Tree | Leaf Int deriving Show

f :: Tree -> (Int, Int)
f t = execState (go t) (0, 0) where

  go :: Tree -> State (Int, Int) ()
  go (Leaf n) = do
    (sum1, sum2) <- get  -- állapot
    if n < 0
      then put (n + sum1, sum2)
      else put (sum1, n + sum2)
  go (Node l r) =
    go l >> go r


-- Parser ismétlés
--------------------------------------------------------------------------------

                                       -- Nothing: parse hiba
                                       -- Just (a, str): olvastunk a-t, maradék str
                                       -- Parser () : csak validál
                                       -- Parser a  : olvas is valamilyen adatot
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- runParser-el használjuk a Parser-eket
-- pl. runParser p input :: Maybe (a, String)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)

  -- egymás után olvasni   (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  -- második olvasás függ az első visszatérési értékétől     (nem függ: (>>), (*>))
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

-- akkor sikeres, ha üres az input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- akkor sikeres, ha az input nemüres, és True-t ad egy függvény az első
-- Char-ra
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

-- olvasunk egy konkrét Char-t az input elejéről
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy tetszőleges karaktert
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- olvassunk egy konkrét String-et:
string :: String -> Parser ()
string str = mapM_ char str    -- minden karaktert olvassunk sorban

-- Alternative f ~ minden a-ra Monoid (f a)
instance Alternative Parser where

  empty = Parser $ \_ -> Nothing

  -- p1 <|> p2 : először p1-et próbálja, utána p2-t
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- whitespace olvasás: olvass nulla vagy több szóközt vagy newline-t
spaceChar :: Parser ()
spaceChar = char ' ' <|> char '\n'


-- parser iterálás: many, some
-- Control.Applicative-ból

-- many e ~ e*          -- nullaszor vagy többször futtatja e-t, és az eredményeket listában adja vissza
-- some e ~ e+          -- legalább egyszer futtatja e-t, és az eredményeket listában adja vissza

-- whitespace olvasás
ws :: Parser ()
ws = () <$ many spaceChar


-- standard függvény: nemüres szeparált sorozat olvasása
-- sepBy1 pa psep  :   pa psep pa psep ..... pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- szeparált sorozat olvasás (lehet üres)
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []


-- Feladatok
--------------------------------------------------------------------------------

-- token: alapvető lexikális elem (literál, azonosító, kulcssszó, operátor, stb.)
-- token parser: token-t olvas, maga után megeszi a whitespace-t

-- Definiáld a char, string függvények token verzióját:
-- (konvenció: token parser '-s)
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws


-- implementáld a következő regex-eket Parser-el!
-- string1=10,string2=20, .....
-- példa: foo=10,bar=20
-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*


many_ p = () <$ many p
some_ p = () <$ some p

lowerCases :: Parser ()
lowerCases = some_ (satisfy (\c -> elem c ['a'..'z']))

digits :: Parser ()
digits = some_ (satisfy (\c -> elem c ['0'..'9']))

p0 :: Parser ()
p0 = lowerCases >> char '=' >> digits >> many_ (char ',' >> lowerCases >> char '=' >> digits)
  -- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*

p0' :: Parser ()
p0' = do
  lowerCases
  char '='
  digits
  many_ (char ',' >> lowerCases >> char '=' >> digits)

-- (ac|bd)*
p1 :: Parser ()
p1 = many_ (string "ac" <|> string "bd")


-- optional p = p <|> pure ()

-- -?[0..9]+         -- a -? opcionális '-'-t jelent
p2 :: Parser ()
p2 = (char '-' <|> pure ()) >> some_ (satisfy (\c -> elem c ['0'..'9']))

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = satisfy (\c -> c1 <= c && c <= c2)

-- [a..z]+@foobar.(com|org|hu)
p3 :: Parser ()
p3 = some_ (charRange 'a' 'z') >> string "@foobar." >> (string "com" <|> string "org" <|> string "hu")

-- A (kutya|macska) ((nagyon )+|kicsit|nem) (éhes|szomjas).
p4 :: Parser ()
p4 = undefined -- string "A " >> (string "kutya" <|> string "macska") >>

letter :: Parser ()
letter = () <$ (charRange 'a' 'z' <|> charRange 'A' 'Z')

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p5 :: Parser ()
p5 = letter >> many_ (letter <|> (() <$ charRange '0' '9'))

-- Olvass be egy számjegyet!
digit :: Parser Int
digit = digitToInt <$> charRange '0' '9'    -- Data.Char-ból digitToInt (hex Char-ból számot csinál)

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = do
  ds <- some digit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined


-- köv BEAD: regex-szintű parser feladat


-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- Írj egy parser-t ami, zárójeleket, +-t és pozitív Int literálokat tartalmazó kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

data Exp = Lit Int | Plus Exp Exp deriving Show

pExp :: Parser Exp
pExp = undefined


-- bónusz: írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined







-- Extra (nem anyag!) : lens & traversal
--------------------------------------------------------------------------------

-- bejárunk 0 vagy több rész-struktúrát
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- bejárunk pontosan 1 rész-struktúrát
type Lens s t a b      = forall f. Functor f     => (a -> f b) -> s -> f t

-- first :: Functor f => (a -> f a') -> (a, b) -> f (a', b)
first :: Lens (a, b) (a', b) a a'
first f (a, b) = (\a' -> (a', b)) <$> f a

second :: Lens (a, b) (a, b') b b'
second f (a, b) = (\b' -> (a, b')) <$> f b

newtype I a = I {unI :: a} deriving Functor
newtype K a b = K {unK :: a} deriving Functor

over :: Lens s t a b -> (a -> b) -> s -> t    -- over first (+10) (10, 20) == (20, 20)
over l f s = unI (l (I . f) s)

view :: Lens s t a b -> s -> a
view l s = unK (l K s)

-- microlens package
-- (NEM lens)
-- makeLenses ''MyDataType

leaves :: Applicative f => (Int -> f Int) -> Tree -> f Tree   -- Traversal Tree Tree Int Int
leaves f (Leaf n)   = Leaf <$> f n
leaves f (Node l r) = Node <$> leaves f l <*> leaves f r
