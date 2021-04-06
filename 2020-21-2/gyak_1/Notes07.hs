
{-# LANGUAGE DeriveFunctor #-}

-- Következő BEAD feladat: nem túl bonyolult regex Parser-el

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- (State String + Maybe) monád
-- állapot: String
-- Parser a : függvény, ami String állapotot módosít és adhat vissza hibát
--   Nothing     : parse error (nem sikerült "a" típusú értéket olvasni)
--   Just (a, s) : sikerült a-t olvasni az input elejéről, maradt "s" String olvasás után

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)      -- return a = State $ \s -> (a, s)

  -- runParser (return 10) "fofsodfskodk" == Just (10,"fofsodfskodk")

  -- egymás után futtatás
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

 -- (p1 >> p2)

instance Alternative Parser where
  -- empty :: Parser a              -- rögtön hibázó Parser
  empty = Parser $ \_ -> Nothing


  -- megpróbáljuk az első parser-t, ha az sikertelen, a másodikat
  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

  -- (<|>) asszociatív bináris művelet (Parser a)-n, empty az egységeleme
  -- Megfelel a "e₁|e₂" regex műveletnek   (e1 <|> e2)

   -- e <|> empty = e
   -- empty <|> e = e

-- "elemi" parserek

-- "end of file" : akkor sikeres, ha üres az input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- akkor sikeres ha, nemüres az input, és igaz az első karakterre a feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- konkrét karakter olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c) :: Parser Char

  -- (<$) "konstans fmap"               -- a <$ fb   --> kicseréli a visszatérési értéket egy konkrét értékre
  -- (<$) a fb = (const a) <$> fb

  -- (<$>) ... (<*>) ..     (Applicative: környeztfüggetlen parser)
  --                        (Monad:       környezetfüggő parser)


-- akármilyen karakter olvasása (eof komplemens parsere)
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- pl: min kettő hosszú String-ek felismerése: runParser (anyChar >> anyChar)

-- konkrét String literál olvasása
-- pl: runParser (string "foo") "foobar" == Just ((),"bar")
--     runParser (string "foo") "bar"    == Nothing

string :: String -> Parser ()
string str = mapM_ char str
   -- str  :: String
   -- str  :: [Char]
   -- mapM_ :: (Char -> Parser b) -> String -> Parser ()

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Control.Applicative-ból importálva: many, some

-- megfelel a regex-beli * operátor
--  e*  ~   many e

-- 0-szor vagy többször próbálunk parser-t futtatni
-- many :: Parser a -> Parser [a]


-- megfelel a regex-beli +-nak:
-- 1-szer vagy többször próbálunk parser-t futtatni
-- some :: Parser a -> Parser [a]

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p


-- implementáld a következő regex-eket:
--------------------------------------------------------------------------------

-- (foo|bar)*kutya
p01 :: Parser ()
p01 = many_ (string "foo" <|> string "bar") >> string "kutya"
  -- runParser p01 "foofoofoobarfoobarkutya" == Just ((),"")

-- \[foo(, foo)*\]         -- nemüres ,-vel választott foo lista
p02 :: Parser ()
p02 = char '[' >> string "foo" >> many_ (string ", foo") >> char ']'

-- (ac|bd)*
p1 :: Parser ()
p1 = undefined

-- [c1..c2]
charRange_ :: Char -> Char -> Parser ()
charRange_ c1 c2 = () <$ satisfy (\c -> c1 <= c && c <= c2)

lowercase_ :: Parser ()
lowercase_ = charRange_ 'a' 'z'

-- [a..z]+@foobar\.(com|org|hu)
p2 :: Parser ()
p2 = some_ lowercase_ >> string "@foobar." >> (string "com" <|> string "org" <|> string "hu")

optional_ :: Parser a -> Parser ()
optional_ p = (() <$ p) <|> pure ()              --   e? := e|ε

digit_ :: Parser ()
digit_ = charRange_ '0' '9'

-- -?[0..9]+           -- a -? opcionális '-'-t jelent
p3 :: Parser ()
p3 = optional_ (char '-') >> some_ digit_

uppercase_ :: Parser ()
uppercase_ = charRange_ 'A' 'Z'

identStart = (lowercase_ <|> uppercase_)
identRest  = many_ (lowercase_ <|> uppercase_ <|> digit_)

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p4 :: Parser ()
p4 = identStart >> identRest

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p5 :: Parser ()
p5 = undefined

--------------------------------------------------------------------------------

-- Olvass be számjegyet!
digit :: Parser Int
digit = undefined

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = undefined

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined

-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- Írj egy parser-t ami, zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))

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
