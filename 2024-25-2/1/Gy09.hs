module Gy09 where

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Traversable
import Data.Foldable
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

-- Nagyon hasonló a State-hez
--                        { runState  :: s      ->       (a,       s) }

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  (Parser rP) >>= f = Parser $ \s -> case rP s of
    Nothing -> Nothing
    Just (a, s') -> runParser (f a) s'


-- A Parser és a State két fő különbsége
-- 1. A Parsernek a "belső állapota" mindig string
-- 2. A Parser el tud hasalni (ezt a belső Maybe reprezentálja)
-- Az elhasalásnál megjegyezhetjük miért áltunk meg egy hiba formájában
-- Ekkor a Parsert lehetne ExceptT String (State String) a-val is reprezentálni (később ezzel is lesz)

-- Primitív parserek
-- Olyan parser, amely lenyel egy karaktert a bemenetről és akkor fogad el, ha az adott predikátum teljesül rá
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input -> case input of
  [] -> Nothing
  (x : xs) -> if p x then Just (x, xs) else Nothing

-- Olyan parser, amely akkor fogad el, ha nincs semmi a bemeneten
eof :: Parser ()
eof = Parser $ \input -> case input of
  [] -> Just ((), [])
  (x : xs) -> Nothing

-- Ezekből felépíthetőek egyéb parserek
-- Olyan parser, ami egy adott karaktert parseol
-- Itt irreleváns az, hogy mi a kimenet
-- void :: Parser a -> Parser ()
char :: Char -> Parser ()
char c = void $ satisfy (== c)

-- Parseoljunk akármilyen karaktert
anychar :: Parser Char
anychar = satisfy (const True) -- \x -> True

-- Parseoljunk egy konkrét stringet
-- hint: mapM_
string :: String -> Parser () -- string = mapM_ char = void . traverse char
string [] = return ()
string (c : cs) = char c >> string cs

-- Parsernél fontos a "vagy" művelet (ha az első parser elhasal, akkor a másikat próbáljuk meg)
-- Ez lesz az Alternative típusosztály
instance Alternative Parser where
  empty :: Parser a -- Garantáltan elhasaló parser
  empty = Parser $ const Nothing -- satisfy (const False)
  (<|>) :: Parser a -> Parser a -> Parser a -- Ha a baloldali sikertelen, futtassuk le a jobboldalit (hint: A Maybe is egy alternatív)
  (Parser pl) <|> (Parser pr) = Parser $ \s -> case pl s of
    Nothing -> pr s
    Just x -> Just x

-- Definiáljunk egy parsert ami egy 'a' vagy egy 'b' karaktert parseol

aorb :: Parser Char
aorb = ('a' <$ char 'a') <|> ('b' <$ char 'b')

-- many :: Parser a -> Parser [a]
-- 0 vagy többször lefuttatja a parsert
-- some :: Parser a -> Parser [a]
-- 1 vagy többször lefuttatja a parsert

many' :: Parser a -> Parser [a]
many' p = some' p <|> pure [] -- Ha 1 vagy több sikertelen, akkor 0 van

some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> many' p -- Lefuttatja 1x és utána 0 vagy többször

-- optional' :: Parser a -> Parser (Maybe a)
-- ha elhasalna a parser, mégse hasal el
optional' :: Parser a -> Parser (Maybe a)
optional' p = do
  -- <$> :: (a -> b) -> Parser a -> Parser b
  --              V Parser a
  a <- (Just <$> p) <|> return Nothing
  --             ^ a
  --   | Just a    |
  return a

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: Integral i => i -> Parser a -> Parser [a]
replicateM' 0 p = return [] -- return :: a -> Parser a
replicateM' i p = do -- (:) <$> p <*> replicateM' (i - 1) ps
  -- legalább 1 darab p-t be kell parseolni
  p' <- p
  ps <- replicateM' (i - 1) p
  return (p' : ps)

-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' [] = empty
asum' (x : xs) = x <|> asum' xs
--                  ^ ha a baloldal nem fut le, akkor a jobboldal

-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anychar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    ℓ{n,}    - Parseol n vagy több darab ℓ kifejezést replicateM n ℓ >> many ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

-- Írjunk regex kifejezéseket parserekkel!

-- alm(a|ák)
p1 :: Parser ()
p1 = do
  string "alm"
  string "a" <|> string "ák"

-- c(i+)ca
p2 :: Parser ()
p2 = do
  char 'c'
  some (char 'i')
  string "ca"

-- (c*)i?(c+)a{3}
p3 :: Parser ()
p3 = do
  many (char 'c')
  optional (char 'i')
  some (char 'c')
  replicateM_ 3 (char 'a') -- void (replicateM 3 (char 'a'))

-- (alma|banana)?
p4 :: Parser ()
p4 = void $ optional (string "alma" <|> string "banana")

-- [A-Z]{10}
p5 :: Parser ()
p5 = replicateM_ 10 $ satisfy (\c -> c >= 'A' && c <= 'Z')

-- \d{2}
p6 :: Parser ()
p6 = void $ replicateM 2 $ satisfy isDigit

-- \d+.*$
p7 :: Parser ()
p7 = do
  some $ satisfy isDigit
  many anychar
  eof

-- \d?alma.?banana
p8 :: Parser ()
p8 = undefined

-- (ab)?ba+.*
p9 :: Parser ()
p9 = undefined

-- \d{2,}-?$
p10 :: Parser ()
p10 = undefined

-- [A-Z]*[1,2,3,9]?(A|Z)$
p11 :: Parser ()
p11 = undefined

-- a+b*c?d{4}e{5,}
p12 :: Parser ()
p12 = undefined

-- alm(a|ák)|banán(ok)?
p13 :: Parser ()
p13 = undefined
