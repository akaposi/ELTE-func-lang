{-# LANGUAGE DeriveFunctor #-}

-- Parser: ismétlés, regex impl, Alternative, kombinátorok, környezetfüggő/független,
-- lista literál, csv, whitespace/token parsing, precedencia

import Control.Monad
import Control.Applicative
import Data.Char

                                  -- output: Nothing       : parser error
                                  --         Just (a, str) : sikeresen olvastunk a-t, maradék str olvasás után
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- (parser kombinátor)
-- (algoritmus: rekurzív leszállás)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where

  -- nem olvasunk semmit, csak visszaadjuk a-t
  return a = Parser $ \s -> Just (a, s)

  -- egymás utáni olvasás
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- input végét olvassa
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- input elejéről egy Char-t olvasunk, ha Bool feltétel teljesül
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs -> if f c then Just (c, cs) else Nothing
  []   -> Nothing

-- bármilyen Char
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét Char
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- konkrét String olvasása
string :: String -> Parser ()
string str = mapM_ char str     -- mapM_ balról jobbra meghívja a char-t az str karaktereire
                                -- type String = [Char]

-- választás parserek között

-- class Applicative f => Alternative f where
--   empty :: f a                    -- monoid: mempty
--   (<|>) :: f a -> f a -> f a      -- monoid: (<>)
--   empty <|> p = p
--   p <|> empty = p
--   + asszociatív <|>

-- Alternative f ~ minden a-ra Monoid (f a)

instance Alternative Parser where
  empty = Parser $ \s -> Nothing

  -- (lehetne joobról balra is próbálni)
  -- (egymást kizáró parsereknél mindegy a sorrend)
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

  -- példa: string "foo" <|> string "bar"      (egymást kizárják, sorrend mindegy)
  --        string "foobar" <|> string "foo"   (átfedő parserek, sorrend számít!)

-- regexek implementációja:

-- ε     ~   pure ()
-- e₁e₂  ~   e₁ >> e₂
-- c     ~   char c
-- e₁|e₁ ~   e₁ <|> e₂
-- $     ~   eof
-- e₁*   ~   many e₁
-- e₁+   ~   some e₁


-- iteráció: parser-t ismételten futtatunk

-- standard függvény: Control.Applicative: many (*), some (+)

-- 0 vagy többször olvas, eredmények egy listában visszaadva
many' :: Alternative f => f a -> f [a]
many' fa = some' fa <|> pure []
       -- min 1 olvasás  <|>  0 olvasás

-- 1 vagy többször olvas, eredmények egy listában visszaadva
some' :: Alternative f => f a -> f [a]
some' fa = (:) <$> fa <*> many' fa


-- példa:
-- runParser (many (char 'x')) "xxxxxxxxxxxxxxxxxxkkkkgdkfgodkfo" ==
--   Just ([(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()],"kkkkgdkfgodkfo")

-- hatékonyabb tud lenni: many_ :: Alternative f => f a -> f ()   (some_ hasonlóképpen)


-- példa: parser, ami nem regex
--------------------------------------------------------------------------------

-- Monad bind-ot használjuk: környezetfüggő olvasás/nyelvtan
-- Applicative metódusokat használjuk: környezetfüggetlen olvasás/nyelvtan
-- n darab 'a' után n darab 'b'

ab :: Parser ()
ab = do
  l <- many (char 'a')  -- l :: [()]
  replicateM_ (length l) (char 'b')

-- runParser ab "aaabbbb" == Just ((),"b")
-- runParser ab "aaaabbb" == Nothing


-- a*b*
ab' :: Parser ()
ab' = () <$ (many (char 'a') *> many (char 'b'))

-- applikatív parser applikatív kombinátorokkal:
-- (<$>)
-- (<*>)
-- (<$)    -- kicseréljük a végeredményt
-- (<*)    -- p1 <* p2 :  egymás után futtat, p1 eredményét adja vissza
-- (*>)    -- p1 *> p2 :  egymás után futtat, p2 eredményét adja vissza

p1 :: Parser (Int, Int, Char)
p1 = (,,) <$> (length <$> many (char 'a'))
          <*> (length <$> many (char 'b'))
          <*> ('y' <$ char 'y')

-- runParser p1 "aaaaaabbbbbbby" == Just ((6,7,'y'),"")


-- [Int] literál olvasása, példa: [   13123, 312312, 4343   ,   5  ]

-- szám karakter olvasása
digit :: Parser Int
digit = digitToInt <$> satisfy (\c -> '0' <= c && c <= '9')

-- szám literál
posInt :: Parser Int
posInt = do
  ds <- some digit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- whitespace olvasás
ws :: Parser ()
ws = () <$ many (char ' ')


intList :: Parser [Int]
intList = ws *> char '[' *> ws *> entries <* char ']' <* ws where   -- amire a csőrök mutatnak, annak az értékét adjuk vissza

  -- entries :: Parser [Int]
  -- entries = ((:) <$> (posInt <* ws) <*> many (char ',' *> ws *> posInt <* ws))
  --        <|> pure []

  entries :: Parser [Int]
  entries = sepBy (posInt <* ws) (char ',' *> ws)



-- 1. szeparált olvasást kifaktoráljuk
-- standard függvények: sepBy, sepBy1

-- egy vagy több a-t olvas, sep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

-- nulla vagy több a-t olvas, sep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []


-- 2. Whitespace strukturált kezelése
--    minden alapvető parser, ami inputot fogyaszt, "token"-t olvas, az maga után megeszi a whitespace-t
--    (kód többi részében nem kell már ws-el foglalkozni)
--    (token parsolás)

-- definiáljuk a "token" verziókat (char, posInt)

char' :: Char -> Parser ()
char' c = char c <* ws

posInt' :: Parser Int
posInt' = posInt <* ws

-- a token konvenció nem kezeli a top-level parserben a kezdő ws-t
-- + nem várja eof-ot az input végén
topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

intList2 :: Parser [Int]
intList2 = topLevel (char' '[' *> sepBy posInt' (char' ',') <* char' ']')

-- csv példa:
-- egy sorban lehet vesszővel elválasztva posInt vagy pedig lowercase string
-- nulla vagy több sor egy fájl

lowerCaseString' :: Parser String
lowerCaseString' = some (satisfy (\c -> 'a' <= c && c <= 'z')) <* ws

csvLine :: Parser [Either Int String]
csvLine = sepBy ((Left <$> posInt') <|> (Right <$> lowerCaseString')) (char' ',')

csv :: Parser [[Either Int String]]
csv = topLevel (sepBy csvLine (char' '\n'))


--------------------------------------------------------------------------------
