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
satisfy p = Parser $ \inputString -> case inputString of
  [] -> Nothing
  (c:cs) -> if p c then Just (c, cs) else Nothing

-- Olyan parser, amely akkor fogad el, ha nincs semmi a bemeneten
eof :: Parser ()
eof = Parser $ \is -> case is of
  [] -> Just ((), []) -- pure []
  _  -> Nothing

-- Ezekből felépíthetőek egyéb parserek
-- Olyan parser, ami egy adott karaktert parseol
-- Itt irreleváns az, hogy mi a kimenet
char :: Char -> Parser ()
char c = do -- void $ satisfy (== c)
  satisfy (== c)
  return ()

-- Parseoljunk akármilyen karaktert
anychar :: Parser Char
anychar = satisfy (const True)

-- Parseoljunk egy konkrét stringet
-- hint: mapM_
string :: String -> Parser ()
string [] = return ()
string (x:xs) = do
  char x
  string xs

-- string i = mapM_ char i
-- string i = void (mapM char i)


-- Parsernél fontos a "vagy" művelet (ha az első parser elhasal, akkor a másikat próbáljuk meg)
-- Ez lesz az Alternative típusosztály
instance Alternative Parser where
  empty :: Parser a -- Garantáltan elhasaló parser
  empty = Parser $ const Nothing
  
  (<|>) :: Parser a -> Parser a -> Parser a -- Ha a baloldali sikertelen, futassuk le a jobboldalit (hint: A Maybe is egy alternatív)
  p1 <|> p2 = Parser $ \iS -> case runParser p1 iS of
    Nothing -> runParser p2 iS
    x       -> x

-- Definiáljunk egy parsert ami egy 'a' vagy egy 'b' karaktert parseol

aorb :: Parser Char
aorb = satisfy (== 'a') <|> satisfy (== 'b')
  -- ('a' <$ char 'a') <|> ('b' <$ char 'b')

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
  a <- (Just <$> p) <|> return Nothing
  return a

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: Integral i => i -> Parser a -> Parser [a]
replicateM' 0 p = return [] -- pure []
replicateM' i p = (:) <$> p <*> replicateM' (i - 1) p

-- <$> = fmap
-- <*> = ap
-- <|> = alt
-- <$ = const fmap
-- <* = rap
-- *> = lap
-- >>= = bind
-- >> = const bind



-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' [] = empty
asum' (p:ps) = p <|> asum' ps -- foldr (<|>) empty 

-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    s        - Parseol egy s stringet                 string s
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anychar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    ℓ{n,}    - Parseol n vagy több ℓ kifejezést       replicateM n ℓ >> many ℓ 
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

-- Írjunk regex kifejezéseket parserekkel!

-- alm(a|ák)
p1 :: Parser ()
p1 = do
  string "alm"
  char 'a' <|> string "ák"

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
  replicateM' 3 (char 'a')
  return ()

-- (alma|banana)?
p4 :: Parser ()
p4 = void $ optional $ string "alma" <|> string "banana"

-- [A-Z]{10}
p5 :: Parser ()
p5 = do
  replicateM' 10 (asum' [char c | c <- ['A'..'Z']])
  pure ()

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

-- [A-Z]*(1|2|3|9)?(A|Z)$
p11 :: Parser ()
p11 = undefined

-- a+b*c?d{4}e{5,}
p12 :: Parser ()
p12 = undefined

-- alm(a|ák)|banán(ok)?
p13 :: Parser ()
p13 = (do -- string "alm" >> (char 'a' <|> string "ák")
          string "alm"
          char 'a' <|> string "ák"
      ) <|> (do -- string "banán" >> (void $ optional (string "ok"))
                string "banán"
                void $ optional (string "ok")
                )
