
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Char  -- isAlpha, isDigit, isAlphaNum, isLower, isUpper, isSpace

--------------------------------------------------------------------------------
-- következő canvas feladat:
--   regex mint Parser definíció

-- canvas feladat
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node2 (Tree a) (Tree a) | Node3 (Tree a) (Tree a) (Tree a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

label :: Tree a -> Tree (Int, a)
label t = evalState (traverse go t) 0 where
  go :: a -> State Int (Int, a)
  go a = do
    n <- get
    put (n + 1)
    pure (n, a)

label' :: Tree a -> Tree (Int, a)
label' t = evalState (go t) 0 where
  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure (Leaf (n, a))
  go (Node2 t1 t2) =
    Node2 <$> go t1 <*> go t2
  go (Node3 t1 t2 t3) =
    Node3 <$> go t1 <*> go t2 <*> go t3


-- State monád
--------------------------------------------------------------------------------

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


-- Parser library
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)   -- nincs hatás

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = mapM_ char s         -- egymás után olvasom az összes Char-t a String-ben


instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
--   optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
--   optional pa = (Just <$> pa) <|> pure Nothing

-- 0 vagy 1 eredményt olvasunk
optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)


-- token/whitespace parsing segédfüggvények

ws :: Parser ()
ws = many_ (satisfy isSpace)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- operátor segédfüggvények

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty


-- FELADATOK
--------------------------------------------------------------------------------

{-

emlékeztető:
  pa       :: Parser a
  () <$ pa :: Parser ()


Megfelelés Parser () és regex között:

  eof          $
  e1|e2        e1 <|> e2
  e*           many_ e
  e+           some_ e
  e?           optional_ e
  c            char c
  (string)     string str
  e1e2         e1 *> e2         e1 >> e2          do {e1; e2}
  [c1..c2]     TODO
-}


-- Implementáld a következő regex-eket! Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
-- példák:   kutya fookutya barfookutya
p1 :: Parser ()
p1 = many_ (string "foo" <|> string "bar") *> string "kutya"

-- \[foo(, foo)*\]     (nemüres ,-vel választott "foo" lista)
-- példák:   [foo] [foo, foo] [foo, foo, foo]
p2 :: Parser ()
p2 = char '[' *> string "foo" *> many_ (string ", foo") *> char ']'

-- (ac|bd)*
-- példák:   acac  acbdbd
p3 :: Parser ()
p3 = many_ (string "ac" <|> string "bd")

range :: Char -> Char -> Parser ()
range c1 c2 = () <$ satisfy (\c -> c1 <= c && c <= c2)

-- (b <$ pa) = fmap (\_ -> b) pa


-- [a..z]+@foobar\.(com|org|hu)
-- példák:   kutya@foobar.org  macska@foobar.org
p4 :: Parser ()
p4 =    some_ (range 'a' 'z')
     *> string "@foobar."
     *> (string "com" <|> string "org" <|> string "hu")

-- p4' = do
--   some_ (range 'a' 'z')
--   string "@foobar."
--   (string "com" <|> string "org" <|> string "hu")

pDigit :: Parser ()
pDigit = range '0' '9'

pLower :: Parser ()
pLower = range 'a' 'z'

pUpper :: Parser ()
pUpper = range 'A' 'Z'

pLetter :: Parser ()
pLetter = pLower <|> pUpper

-- -?[0..9]+
-- (?e azt jelenti, hogy e opcionális)
p5 :: Parser ()
p5 = optional_ (char '-') *> some_ pDigit

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p6 :: Parser ()
p6 = pLetter *> many_ (pLetter <|> pDigit)

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példák:  foo=10,bar=30,baz=40
p7 :: Parser ()
p7 = let
  pInner = some_ pLower *> char '=' *> some_ pDigit
  in pInner *> many_ (char ',' *> pInner)

p7' :: Parser ()
p7' = () <$ sepBy (some_ pLower *> char '=' *> some_ pDigit) (char ',')


--------------------------------------------------------------------------------


-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = undefined

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = undefined

-- Írj egy parsert, ami [Maybe Int] értékeket olvas be Haskell szintaxis szerint!
-- Engedj meg bárhol whitespace-t.
listMaybeInt :: Parser [Maybe Int]
listMaybeInt = undefined


-- Írj egy parsert, ami [(Bool, Maybe Int)] értékeket olvas Haskell szintaxis szerint!
-- Engedj meg bárhol whitespace-t.
listBoolMaybeInt :: Parser [(Bool, Maybe Int)]
listBoolMaybeInt = undefined


-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = undefined


-- Írj egy parser-t, ami zárójeleket, +-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! (Lásd előadás) Whitespace-t mindenhol engedj meg.
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


-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
