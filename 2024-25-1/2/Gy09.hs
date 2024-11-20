{-# LANGUAGE LambdaCase #-}
module Gy07 where

import Control.Monad
import Data.Functor
import Data.Traversable
import Data.Foldable
import Data.Char
import Control.Monad.State
import Control.Monad.Except

-- Parser hibaüzenettel
type Parser  = StateT String (Except String)

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

-- Primitívek

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

-- Definiáljunk egy parsert ami egy 'a' vagy egy 'b' karaktert parseol

aorb :: Parser Char
aorb = undefined

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
optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: Integral i => i -> Parser a -> Parser [a]
replicateM' = undefined

-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' = undefined

-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

-- Írjunk regex kifejezéseket parserekkel!

-- alm(a|ák)
p1 :: Parser ()
p1 = undefined

-- c(i+)ca
p2 :: Parser ()
p2 = undefined

-- (c*)i?(c+)a{3}
p3 :: Parser ()
p3 = undefined

-- (alma|banana)?
p4 :: Parser ()
p4 = undefined

-- [A-Z]{10}
p5 :: Parser ()
p5 = undefined

-- \d{2}
p6 :: Parser ()
p6 = undefined

-- \d+.*$
p7 :: Parser ()
p7 = undefined

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
