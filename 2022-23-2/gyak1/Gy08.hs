{-# LANGUAGE InstanceSigs, DeriveFunctor #-}

module Gy08 where

import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Traversable
import Data.Foldable
import Data.Char

{-# ANN module "HLint: ignore Use lambda-case" #-}
{-# ANN module "HLint: ignore Use asum" #-}
{-# ANN module "HLint: ignore Use optional" #-}

-- Parser monád: Szöveget alakít át valami mássá

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor
-- Nagyon hasonló a State-hez
--                        { runState  :: s      ->       (a,       s) }

-- A Parser és a State két fő különbsége
-- 1. A Parsernek a "belső állapota" mindig string
-- 2. A Parser el tud hasalni (ezt a belső Maybe reprezentálja)

-- Ezt az elhasalást le kell kezelnünk, ez lesz az Alternative típusosztály

instance Applicative Parser where
  (<*>) = ap
  pure a = Parser $ \s -> Just (a, s)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, s') -> runParser (f a) s'


-- Elemi parserek:

-- Megnézi, hogy a következő karakterre igaz-e a predikátum
satisfy :: (Char -> Bool) -> Parser Char -- Visszaadja mellékhatásba a karaktert amire teljesül
satisfy p = Parser $ \s -> case s of
  (x:xs) | p x -> Just (x, xs) -- leszedjük a fejelemet
  _            -> Nothing      -- elhasal ha nincs több, vagy nem igaz a predikátum

-- Megnézi nincs-e több karakter
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing


-- Példaparserek
-- Írjunk egy parsert ami beparseol egy 'a' betűt vagy egy 'b' betűt
-- Segítség: void :: Functor f => f a -> f ()
aOrB :: Parser ()
aOrB = () <$ satisfy (\c -> c == 'a' || c == 'b')

-- Írjunk egy parser ami beparseol egy kisbetűt!
lowercaseChar :: Parser ()
lowercaseChar = () <$ satisfy isLower


-- Egyéb 'elemi' parserek
-- Parseoljunk be akármilyen karaktert
anyChar :: Parser Char
anyChar = satisfy (const True)

-- Parseoljunk be egy specifikus karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (== c)


-- Új típusosztály: Alternative ("Hibakezelés" típuosztály)

-- empty -> olyan parser ami biztosan elhasal
-- a <|> b -> ha a elhasal, akkor futtasd le b-t ugyanarra a bemenetre
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

-- Alternative instance-a pl az IO monádnak is van
-- (readLn :: IO Int) <|> pure 2

-- Olvassunk be vagy egy 'a' betűt vagy egy 'b' betűt. Használjunk <|>-t!
aOrB' :: Parser ()
aOrB' = void $ satisfy (\c -> c == 'a') <|> satisfy (\c -> c == 'b')


-- A Parser is egy monád

-- Olvassunk be egy 'a'-t majd egy 'b'-t
aThenB :: Parser ()
aThenB = do
  satisfy (== 'a')
  satisfy (== 'b')
  pure ()

-- Olvassunk be két adott karaktert egymás után
twoChars :: Char -> Char -> Parser ()
twoChars c1 c2 = do
  char c1--satisfy (== c1)
  char c2--satisfy (== c2)
  pure ()

-- Komolyabb parserek

-- Olvassunk be egy adott stringet!
string :: String -> Parser ()
string [] = pure ()
string (x:xs) = do
  char x
  string xs


-- Parser segédfüggvények

-- Alternative-val járók:

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
optional' p = Just <$> p <|> pure Nothing

-- mapM, traverse :: (a -> Parser b) -> [a] -> Parser [b]
-- Lefuttat egy lista alapján parsereket

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: Integral i => i -> Parser a -> Parser [a]
replicateM' i p
  | i <= 0 = pure []
  | otherwise = (:) <$> p <*> replicateM' (i - 1) p


-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' = foldr (<|>) empty

-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    ℓ₁ℓ₂     - Parser szekvencia                      ℓ₁ >> ℓ₂ vagy do { ℓ₁; ℓ₂; }
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    ℓ{n,}    - Parseol n vagy több darab ℓ-t          replicateM n ℓ >> many ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

-- A fenti parserek kicsit erősebbek mint a regex parserei, képesek Chomsky 2-es (Kontextusfüggetlen) nyelvtanra
-- Lehet Chomsky 1-es (kontextusfüggő) nyelvtan parsert írni, de azt nem fogunk (erre példa pl, indentációfüggőség)

-- Írjunk regex kifejezéseket parserekkel!

-- alm(a|ák)
p1 :: Parser ()
p1 = do
  string "alm"
  void $ string "a" <|> string "ák"-- alt

-- c(i+)ca
p2 :: Parser ()
p2 = do
  char 'c'
  some $ char 'i'
  string "ca"

-- (c*)i?(c+)a{3}
p3 :: Parser ()
p3 = many (char 'c') *> optional (char 'i') *> some (char 'c') *> replicateM_ 3 (char 'a')

-- (alma|banana)?
p4 :: Parser ()
p4 = void $ optional (string "alma" <|> string "banana")

-- [A-Z]{10}
p5 :: Parser ()
p5 = do
  replicateM_ 10 $ satisfy isUpper

-- \d{2}
p6 :: Parser ()
p6 = replicateM_ 2 digit where
  digit = satisfy isDigit

-- \d+.*$
p7 :: Parser ()
p7 = do
  some $ satisfy isDigit
  many anyChar
  eof

-- \d?alma.?banana
p8 :: Parser ()
p8 = do
  optional $ satisfy isDigit
  string "alma"
  optional anyChar
  string "banana"


-- (ab)?ba+.*
p9 :: Parser ()
p9 = void $ (void $ optional $ string "ab") *> char 'b' *> some (char 'a') *> many anyChar

-- \d{2,}-?$
p10 :: Parser ()
p10 = do
  replicateM_ 2 $ digitToInt <$> satisfy (== 'd')
  optional (string "-")
  eof

-- [A-Z]*[1,2,3,9]?(A|Z)$
p11 :: Parser ()
p11 = many (satisfy (\c -> c <= 'Z' && c >= 'A')) *> optional (char '1' <|> char '2' <|> char '3' <|> char '9') *> (char 'A' <|> char 'Z') *> eof

-- a+b*c?d{4}e{5,}
p12 :: Parser ()
p12 = do
  some $ char 'a'
  many $ char 'b'
  optional $ char 'c'
  replicateM 4 $ char 'd'
  replicateM 5 $ char 'e'
  void $ many $ char 'e'

-- alm(a|ák)|banán(ok)?
p13 :: Parser ()
p13 = undefined
