
{-# LANGUAGE DeriveFunctor #-}

import Control.Monad
import Control.Applicative   -- innen jön az Alternative, many
import Data.Char

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

-- listába bettenni a korábbi elemek összegeit
--------------------------------------------------------------------------------

sumList :: [Int] -> [Int]
sumList ns = evalState (sumList' ns) 0

sumList' :: [Int] -> State Int [Int]
sumList' ns = mapM go ns where
  go :: Int -> State Int Int
  go n = do
    sum <- get
    put (sum + n)
    pure sum

sumList'' :: [Int] -> State Int [Int]
sumList'' []     = pure []
sumList'' (n:ns) = do
  sum <- get
  let sum' = sum + n
  put sum'
  ns' <- sumList'' ns
  pure (sum:ns')

-- State nélkül:
-- f :: Int -> [Int] -> [Int]
-- f sum []     = []
-- f sum (n:ns) = sum : f (sum + n) ns



-- Parser
--------------------------------------------------------------------------------

-- minél szebb Parser programot szeretnénk írni
--    1. kézzel írt rekurzív parser (C++ fordítók: mind ezt használják, clang/gcc)
--    2. parser generátor (deklaratív, gyors, többértelműségek/hibaüzenetek kezelése nehéz, nehézkes fejlesztés)
--    3. parser kombinátor (rekurzív parserek, viszont tömörebb/deklaratívabb formában)
--                          nincs kódgenerálás : kényelmes fejlesztés
--                          nincs kódgenerálás : lassab generált parserek  (gyors lehet: de kézzel kell optimalizálni)


newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}


   -- függvény eredmény: Nothing     = parse error
   --                  : Just (a, s) = sikeresen egy "a" típusú értéket olvastunk az input elejéről
   --                                  "s": fennmaradó input
   --                                  egymás után végrehajtás: (>>=), (>>)
   --                                  fmap : végeredményre alkalmazunk egy függvényt
   --                                  pure : nem olvas semmi inputot, rögtön visszatér egy "a" értékkel

   -- Parser = State String + Maybe

instance Functor Parser where
  -- f   :: a -> b
  -- g   :: String -> Maybe (a, String)
  -- cél :: String -> Maybe (b, String)
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing      -> Nothing
    Just (a, s') -> Just (f a, s')

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  -- nem olvasunk, nem hibázunk
  return a = Parser $ \s -> Just (a, s)

  -- egymás után parsolás (hiba + állapot propagálás)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing            -- hiba megy tovább
      Just (a, s') -> runParser (g a) s' -- új állapottal hívjuk (g a)-t

-- akkor sikeres, ha üres az input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- akkor sikeres, ha az input nemüres, és True-t ad egy függvény az első
-- Char-ra
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)   -- extra feltétel, f c == True
       | otherwise -> Nothing
  [] -> Nothing

-- olvasunk egy konkrét Char-t az input elejéről
char :: Char -> Parser ()
char c = () <$ satisfy (==c)   -- (() <$) :: f a -> f ()     visszatérési típust ()-re állítjuk

-- olvassunk egy tetszőleges karaktert
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- pl: (1000 <$ getLine)

-- runParser (char 'a' >> char 'b' >> char 'c') "abcfoo" == Just ((),"foo")

-- mapM_ char "abc" == char 'a' >> char 'b' >> char 'c' >> return ()

-- olvassunk egy konkrét String-et:
string :: String -> Parser ()
string = mapM_ char    -- minden karaktert olvassunk sorban

-- Applicative Parser példa:
-- runParser ((,) <$> anyChar <*> anyChar) "kokokoko" == Just (('k','o'),"kokoko")

-- Olyan parser-t, ami olvas "kutya"-t vagy "macska"-t

-- Alternative: Monoid változata, Applicative f-ekre
-- class Applicative f => Alternative f where
--    empty :: f a                -- "mempty" megfelelője
---   (<|>) :: f a -> f a -> f a  -- "choice" (<>) megfelelője

-- "Alternative f" azt jelenti, hogy "Monoid (f a)" tetszőleges a-ra

instance Alternative Parser where
  -- hibázó parser
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  -- ha az első parser hibázik, futtatjuk a másodikat
  -- egyébként visszaadjuk az első parser eredményét
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

  -- p <|> empty = p
  -- empty <|> p = p
  -- p <|> (q <|> r) = (p <|> q) <|> r

-- olvasson kutya/macska-t
p1 :: Parser ()
p1 = string "kutya" <|> string "macska"


-- reguláris kifejezések reprodukálása: kell még *

-- Futassunk egy parser-t 0-szor vagy többször, adjuk vissza listában az
-- összes eredményt
-- standard: many :: Alternative f => f a -> f [a]
many' :: Parser a -> Parser [a]
many' pa = some' pa        <|>    pure []
  --  legalább 1 olvasás         nulla olvasás (üres lista eredmény)

-- reguláris + operátor
some' :: Parser a -> Parser [a]
some' pa = (:) <$> pa <*> many' pa
    -- olvassunk egy a-t, utána pedig nulla vagy több a-t

-- tetszőleges regex-et írhatunk

-- runParser (() <$ many (char 'x')) "xxxxxxxxxxxxxxxxxxxxxxxxkkkkkkkkkkkkkkkkkk" == Just ((),"kkkkkkkkkkkkkkkkkk")

p2 :: Parser ()
p2 = () <$ some (string "kutya" <|> string "macska")


--------------------------------------------------------------------------------

-- whitespace olvasás: olvass nulla vagy több szóközt vagy newline-t
-- tipp: Data.Char.isSpace használható
spaceChar :: Parser ()
spaceChar = char ' ' <|> char '\n'

ws :: Parser ()
ws = () <$ many spaceChar

-- min 1 szóközzel elválasztott kutya|macska
p3 :: Parser ()
p3 = () <$ many ((string "kutya" <|> string "macska") >> spaceChar >> ws)

-- nem szép: runParser p3 "kutya    macska   kutya macska" == Just ((),"macska")
-- szeparátor nem kell utolsó elem után?

-- kombinátor: szeparált sorozat olvasása
-- standard függvény: nemüres szeparált sorozat olvasása

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

-- kutya/macska elválasztva vesszővel, és lehet whitespace mindenhol
p4 :: Parser ()
p4 = () <$ sepBy1 (string "kutya" <|> string "macska") (ws >> char ',' >> ws)

-- Haskell szintaxis kutya/macska lista:
-- [ kutya,   macska     ]
p5 :: Parser ()
p5 = char '[' >> ws >> sepBy1 ((string "kutya" <|> string "macska") >> ws) (char ',' >> ws) >> char ']'

-- addig olvas, amíg tud:
-- runParser p5 "[ kutya, macska, kutya, kutya, macska ] dfogkdfogkdofgkdo" == Just (()," dfogkdfogkdofgkdo")

-- Hogyan tudjuk azt elérni, hogy az egész inputot olvassuk?
-- eof illeszkedik az input végére:
p6 :: Parser ()
p6 = p5 >> eof

-- runParser p6 "[ kutya, macska, kutya, kutya, macska ] dfogkdfogkdofgkdo" == Nothing



-- példa nem reguláris parser-re:
--------------------------------------------------------------------------------

-- klasszikus példa: olvassunk valahány 'a'-t, utána ugyanannyi 'b'-t:

-- "interakció" : Monad instance-al megvalósítható

-- köv: BEAD: State feladat
p7 :: Parser ()
p7 = do
  as <- many (char 'a')
  let num = length as
  replicateM_ num (char 'b') -- num-szor b-t olvas

-- runParser p7 "aaaabbbb" == Just
-- runParser p7 "aaaaabbbb" == Nothing

--------------------------------------------------------------------------------


-- Olvass be egy számjegyet!
digit :: Parser Int
digit = undefined

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Int
posInt = undefined


-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


{-
Írj egy egyszerű csv (comma separated values) parsert!
A formátum a következő:
   - az inputban nulla vagy több sor lehet
   - minden sorban vesszővel elválasztva szerepelnek vagy számok vagy pedig
     latin betűt tartalmazó szavak
   - whitespace nem engedett meg az új sorokon kívül
   - olvassuk eof-al az input végét

A parser adja vissza az összes sor listáját, ahol (Either Int String) tárolja
a szavakat vagy számokat.

Példa helyes inputra:

  foo,bar,12,31
  40,50,60,kutya,macska

-}
