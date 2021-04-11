
-- Applikatív parsolás, kombinátorok
-- token parsolás, eof konvenció
-- left-faktorálás, left-rekurzió
-- precedenciák

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor  -- sikeres parse végeredményén függvény alkalmazása

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  -- nem olvasunk, csak visszaadunk egy értéket
  return a = Parser $ \s -> Just (a, s)

  -- egymás után futtatunk két parser-t (a második függhet az első
  -- eredményétől)
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where

  -- rögtön hibázó parser
  empty = Parser $ \_ -> Nothing

  -- először az első parser-t futtatjuk, hiba esetén a másodikat
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

-- 1 vagy több "a"-t olvasunk, "sep"-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)
                      -- a        [a]
                 -- (:) :: a -> [a] -> [a]

-- 0 vagy több "a"-t olvasunk, "set"-el szeparálva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- példa:
--   runParser (sepBy (char 'x') (char ',')) "x,x,x,x,x"
--    == Just ([(),(),(),(),()],"")


-- + Control.Applicative-ból importálva:
--  - many   : nulla vagy több érték olvasása
--  - some   : egy vagy több érték olvasása

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p


-- Applikatív parsolás
--------------------------------------------------------------------------------

-- Alap metódusok
-- (<$>) :: Functor f     =>   (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- pure  :: Applicative f => a -> f a

-- Definiálható kombinátorok (standard)
-- (<$)  :: Functor f     =>         b  -> f a -> f b
-- (*>)  :: Applicative f => f a -> f b -> f b
-- (<*)  :: Applicative f => f a -> f b -> f a

-- (*>) ugyanaz, mint (>>)
rbind :: Applicative f => f a -> f b -> f b
rbind fa fb = (\a b -> b) <$> fa <*> fb

-- (<*)
lbind :: Applicative f => f a -> f b -> f a
lbind fa fb = (\a b -> a) <$> fa <*> fb

-- példák:
-- párok listája

-- word :: Parser String
-- word = some (satisfy isLetter)

-- -- deklaratív stílus
-- wordPair :: Parser (String, String)
-- wordPair =
--   (,) <$> (char '(' *> word <* char ',')
--       <*> (word  <* char ')')

-- -- imperatív stílusú definíció
-- wordPair' :: Parser (String, String)
-- wordPair' = do
--   char '('
--   w1 <- word
--   char ','
--   w2 <- word
--   char ')'
--   pure (w1, w2)

-- -- 0 vagy több wordPair, vesszővel elválasztva, listában
-- wordPairList :: Parser [(String, String)]
-- wordPairList = char '[' *> sepBy wordPair (char ',') <* char ']'

-- környezetfüggő parser
abN :: Parser ()
abN = do
  as <- many (char 'a')
  mapM_ (\_ -> char 'b') as  -- as minden elemére olvasok egy char 'b'-t


-- rekurzív applikatív parser:
-- (nem regex!)
-- kiegyensúlyozott zárójelek

-- ()
-- ()()
-- ((()()))
-- ((()))(())

balancedPar1 :: Parser ()
balancedPar1 = char '(' *> many_ balancedPar1 *> char ')'

-- eof: az egész inputnak helyesnek kell lennie!
balancedPar :: Parser ()
balancedPar = many_ balancedPar1 <* eof

-- balancedPar1 :: Parser ()
-- balancedPar1 = char '(' *> balancedPar0 *> char ')'

-- balancedPar0 :: Parser ()
-- balancedPar0 = many_ balancedPar1

-- balancedPar :: Parser ()
-- balancedPar = balancedPar0 <* eof


-- whitespace kérdése
----------------------------------------------------------------------

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

word :: Parser String
word = some (satisfy isLetter)

-- Nem akarjuk mindig kézzel beszúrni a ws-t!
-- wordPair :: Parser (String, String)
-- wordPair =
--   (,) <$> (char '(' *> ws *> word <* ws <* char ',')
--       <*> (ws *> word  <* ws <* char ')')

-- megoldás: token konvenció:
--   alap parserek: maga után megeszi a ws-t
--   ws-t evő parser: "token" parser

-- felesleges ws: ws *> p1 *> ws *> ws *> p2 *> ws
--                p1 *> ws *> p2 *> ws

--  fontos: egész input elejéről le kell venni a ws-t

-- token verzió:
word' :: Parser String
word' = some (satisfy isLetter) <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

wordPair :: Parser (String, String)
wordPair =
  (,) <$> (char' '(' *> word' <* char' ',')
      <*> (word' <* char' ')')

-- topLevel parser:
--   - kezdő ws-t olvassuk
--   - parsolunk
--   - eof-t olvasunk

-- miért maga után ws-ezik?      (kicsit hatékonyabb)
--       (string' "foo" *> p1)
--   <|> (string' "bar" *> p2)
--   <|> (string' "yyy" *> p3)

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof


-- indentation parsing:

-- newtype Parser' a = Parser' {
--   runParser' :: String -> Int -> Int -> Maybe (a, String, Int)
--   } deriving Functor

-- 1. Int param: "elvárt" indentációs szint
-- 2. Int param: extra állapot komponens     (State (String, Int))
--             nyilván tartja, hogy mennyi ws-t olvastunk a legutóbbi newline óta

-- def f():
--    foo
--    bar

-- Token konvenció:
--   minden token parser:
--      először olvas ws-t, utána check-olja a tényleges indentációt az elvárthoz


-- left-factoring / left-recursion
-- "bal-faktorálás" / "bar-rekurzió"
--------------------------------------------------------------------------------

-- left-factoring:

-- ((p1 *> p2) <|> (p1 *> p3)) = (p1 *> (p2 <|> p3))
-- (char 'x' *> char 'a') <|> (char 'x' *> char 'b')
-- char 'x' *> (char 'a' <|> char 'b')

-- left-recursion: ha parser valamilyen ágon saját magát rekurzívan hívja
--   *első műveletként*, az végtelen loop

-- list "végét" tudjuk kiegészíteni
data SnocList a = Nil | Snoc (SnocList a) a
  deriving (Show)

sl1 :: SnocList Int
sl1 = ((Nil `Snoc` 10) `Snoc` 20) `Snoc` 30

-- végtelen loop!
many' :: Parser a -> Parser (SnocList a)
many' pa = (Snoc <$> many' pa <*> pa) <|> pure Nil

-- tail rekurzió-t kell használni.
many'' :: Parser a -> Parser (SnocList a)
many'' pa = go Nil where
  go acc = (do {a <- pa; go (Snoc acc a)}) <|> pure acc
  -- go acc = (go . Snoc acc =<< pa) <|> pure acc

-- 1. megoldás: írjuk át kézzel a left-recursion-t tail recursion-re
-- 2. megoldás: használjunk library kombinátort erre az esetre (chainl függvény)

-- chain left: olvassunk egy "a"-t, utána olvasunk 0-vagy több "b"-t,
-- és az eredményeket egy függvénnyel kombináljuk (bal asszociatív módon)

-- "foldl" függvény Parser verziója

-- f (f (f (f b a) a) a) a

chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

many''' :: Parser a -> Parser (SnocList a)
many''' pa = chainl Snoc (pure Nil) pa
