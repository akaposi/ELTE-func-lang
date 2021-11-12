
{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable,
    DeriveTraversable #-}

-- következő óra elei feladat: egyszerű regex

import Data.Foldable
import Data.Traversable
import Control.Monad
import Control.Applicative -- many, some
import Data.Char           -- isDigit :: Char -> Bool
                           -- digitToInt :: Char -> Int

import Debug.Trace         -- trace :: String -> a -> a
                           -- traceShow :: Show b => b -> a -> a

import Control.Monad.State  -- (miért nem ezt írjuk?)
                            -- (az itteni definíció bonyolultabb,
                            --  a típushibák nem azok, mint amit mi néztünk)

-- canvas:
data Tree = Leaf Int | Node Tree Tree
  deriving (Show)

f :: Tree -> (Int, Int)
f t = execState (go t) (0, 0) where

  go :: Tree -> State (Int, Int) ()
  go (Leaf n) =
    modify (\(s1, s2) -> if n < 0 then (s1 + n, s2)
                                  else (s1, s2 + n))
  go (Node l r) = go l >> go r

traverseLeaves_ :: Applicative f => (Int -> f ()) -> Tree -> f ()
traverseLeaves_ f (Leaf n)   = f n
traverseLeaves_ f (Node l r) = traverseLeaves_ f l *> traverseLeaves_ f r

-- másik :
--   Applicative f => (Int -> f Int) -> Tree -> f Tree

f' :: Tree -> (Int, Int)
f' t = execState (traverseLeaves_ go t) (0, 0) where

  go :: Int -> State (Int, Int) ()
  go n =
    modify (\(s1, s2) -> if n < 0 then (s1 + n, s2)
                                  else (s1, s2 + n))


-- PARSER LIBRARY
--------------------------------------------------------------------------------

-- Parser a : String-ből "a" típusú értéket próbál olvasni
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- Parser a : State String + Maybe
-- library: kisebb Parser függvényekből nagyobbakat összerakni
--   (parser "kombinátor" library)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where

  -- nem dob hibát + nem fogyaszt inputot
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- egymás után két parsert hívunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- parserek közötti választás
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- üres String olvasása
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- Char olvasása, amire egy feltétel teljesül
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ f = () <$ satisfy f

-- konkrét Char olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- parser hívásakor kiír egy String üzenetet
debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- bármilyen Char olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét String-et próbál olvasni
string :: String -> Parser ()
string = traverse_ char

-- példák instance metódusokkal
--   Functor, Applicative, Monad, Alternative

--  fmap : függvényt alkalmazunk az eredményre
--  Applicative : N aritású függvényt, N darab kimenetre
--  pure a : visszaadja a-t, nem olvas semmit
--  (>>=) : egymás után két parser-t futtatunk
--  Alternative metódusok:
--      empty : rögtön Nothing-ot adó parser
--      (<|>) : próbálunk egy parser-t futtatni, ha hibázik, akkor
--              a másik adott parser-t próbáljuk ("choice")

pr1 = string "kutya" <|> string "macska"
pr2 = (char 'x' <|> char 'y') >> (char 'x' <|> char 'y')
pr3 = replicateM_ 3 (string "kutya")

-- Control.Applicative-ból (iterálás):
-- many :: Parser a -> Parser [a]        -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]        -- 1-szer vagy többször olvasás

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

-- Parser () : "validáló" függvény
--   fenti műveletekkel tetszőleges regex-et definiálhatunk
-- char, <|>, many_, some_, eof, (>>)  (lefedi a klasszikus regex definíciót)
------------------------------------------------------------

-- implementáld a következő regex-eket. Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
p01 :: Parser ()
p01 = many_ (string "foo" <|> string "bar") >> string "kutya"

-- tipp: bármi minta van, ami ismétlődik, akkor
-- azt nyugodtan lehet segéd-kombinátorral megoldani

-- runParser p01 "fookutya" == Just ((),"")

-- \[foo(, foo)*\]         -- nemüres ,-vel választott foo lista
p02 :: Parser ()
p02 = string "[foo" >> many_ (string ", foo") >> char ']'

-- (ac|bd)*
p1 :: Parser ()
p1 = many_ (string "ac" <|> string "bd")

inList_ :: [Char] -> Parser ()
inList_ str = satisfy_ (\c -> elem c str)

inList_' :: [Char] -> Parser ()
inList_' str = foldr (\c p -> char c <|> p) empty str
          -- = () <$ choice (map char) str
-- std függvény:
choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) empty ps

lowercase :: Parser ()
lowercase = satisfy_ (\c -> 'a' <= c && c <= 'z')

-- [a..z]+@foobar\.(com|org|hu)
p2 :: Parser ()
p2 = do
  some_ lowercase
  string "@foobar."
  (string "com" <|> string "org" <|> string "hu")

-- -?[0..9]+           -- a -? opcionális '-'-t jelent
p3 :: Parser ()
p3 = (char '-' <|> pure ()) >> some_ (satisfy isDigit)

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p4 :: Parser ()
p4 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példa elfogadott inputra:   foo=10,bar=30,baz=40
p5 :: Parser ()
p5 = undefined

------------------------------------------------------------

   -- Functor/Applicative operátorok
   --   (<$)       kicserélni parser végeredményét adott értékre
   --   (<$>)      fmap
   --   (<*)       két parser-t futtat, az első értékét visszaadja
   --   (*>)       két parser-t futtat, a második értékét visszaadja

-- whitespace elfogyasztása
ws :: Parser ()
ws = () <$ many (char ' ')

-- Olvassuk pa-t 0-szor vagy többször, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- Olvassuk pa-t 1-szor vagy többször, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- egy számjegy olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit


-- FELADATOK
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

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


-- bónusz (nehéz): írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
