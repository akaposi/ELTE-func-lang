{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

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
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
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
satisfy p = Parser $ \s -> case s of
  []     -> Nothing
  (x:xs) -> if p x then Just ( x , xs ) else Nothing

-- Olyan parser, amely akkor fogad el, ha nincs semmi a bemeneten
eof :: Parser ()
eof = Parser $ \s -> case s of
  []       -> Just ((), [])
  (x : xs) -> Nothing

-- Ezekből felépíthetőek egyéb parserek
-- Olyan parser, ami egy konkrét karakter ad vissza
-- Itt irreleváns az, hogy milyen karakter parseol
char :: Char -> Parser ()

char c = void $ satisfy (c ==)
--char c = void $ satisfy (\c' -> c == c')

-- "almafa"
-- char 'a'
-- "lmafa"
-- ()

-- f :: * -> *
-- void :: f a -> f ()

-- <$ :: a -> f b -> f a

-- Parseoljunk akármilyen karakter
anychar :: Parser Char
anychar = satisfy (const True)

-- Parseoljunk egy konkrét stringet
-- hint: mapM_
string :: String -> Parser ()
{-
string []     = return ()
string (x:xs) = do
  char x
  string xs
-}
string xs = mapM_ char xs 

-- Parsernél fontos a "vagy" művelet (ha az első parser elhasal, akkor a másikat próbáljuk meg)
-- Ez lesz az Alternative típusosztály
instance Alternative Parser where
  empty :: Parser a -- Garantáltan elhasaló parser
  empty = Parser $ \s -> Nothing
  (<|>) :: Parser a -> Parser a -> Parser a -- Ha a baloldali sikertelen, futassuk le a jobboldalit (hint: A Maybe is egy alternatív)
  (<|>) pa pa' = Parser $ \s -> case runParser pa s of
    Nothing -> runParser pa' s
    Just r -> Just r

-- Definiáljunk egy parsert ami egy 'a' vagy egy 'b' karaktert parseol

aorb :: Parser ()
aorb = (char 'a' <|> char 'b')

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
optional' p = Parser $ \s -> case runParser p s of
  Nothing      -> Just (Nothing, s)
  Just (x, xs) -> Just (Just x, xs)

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: Integral i => i -> Parser a -> Parser [a]
replicateM' 0 p = return []
replicateM' m p = do
  a <- p
  as <- replicateM' (m-1) p
  return (a:as)

-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' [] = empty
asum' (p:ps) = p <|> asum' ps
-- asum' = foldr (<|>) empty

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

interval :: Char -> Char -> Parser Char
interval c₁ c₂ = satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)

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
  many $ char 'c'
  optional $ char 'i'
  some $ char 'c'
  replicateM 3 $ char 'a'
  return () -- or void $ replicateM 3 $ char 'a'

-- (alma|banana)?
p4 :: Parser ()
p4 = do
  void $ optional $ (string "alma") <|> (string "banana")

-- [A-Z]{10}
p5 :: Parser ()
p5 = do
  void $ replicateM 10 (interval 'A' 'Z')

-- \d{2}
p6 :: Parser ()
p6 = do
  void $ replicateM 2 (digitToInt <$> satisfy isDigit)  

-- \d+.*$
p7 :: Parser ()
p7 = do
  some $ digitToInt <$> satisfy isDigit
  many $ anychar
  eof


-- \d?alma.?banana
p8 :: Parser ()
p8 = do
  optional $ digitToInt <$> satisfy isDigit
  string "alma"
  optional $ anychar
  string "banana"

-- (ab)?ba+.*
p9 :: Parser ()
p9 = do
  optional (string "ab")
  char 'b'
  optional $ char 'a'
  void $ many anychar
  
-- \d{2,}-?$
p10 :: Parser ()
p10 = do
  replicateM 2 (digitToInt <$> satisfy isDigit)
  many (digitToInt <$> satisfy isDigit)
  optional (char '-')
  eof

-- [A-Z]*[1,2,3,9]?(A|Z)$
p11 :: Parser ()
p11 = do
  many $ interval 'A' 'Z'
  optional $ asum [char '1', char '2', char '3',char '9']
  char 'A' <|> char 'Z'
  eof

-- a+b*c?d{4}e{5,}
p12 :: Parser ()
p12 = do
  some $ char 'a'
  many $ char 'b'
  optional $ char 'c'
  replicateM 4 $ char 'd'
  replicateM 5 $ char 'e'
  void $ many $ char '5'

-- alm(a|ák)|banán(ok)?
p13 :: Parser ()
p13 = do
  (do 
    string "alm"
    (char 'a') <|> (string "ák"))
  <|>
  (do
    string "banán"
    void $ optional $ string "ok")


p13' :: Parser ()
p13' = do
  asum 
    [ string "alm" >> (char 'a') <|> (string "ák")
    , string "banán" >>= \_ -> void $ optional $ string "ok" ]

-- Treat
-- A regex parszoláshoz, elég CSAK az applicative + alternative, nem kell monád
-- https://blog.jle.im/entry/free-alternative-regexp.html
infixl 3 `Ap`

data AltF f a where
  Ap     :: f a -> Alt f (a -> b) -> AltF f b
  Pure   :: a                     -> AltF f a

newtype Alt f a = Alt {alternatives :: [AltF f a]} 

instance Functor (AltF f) where
  fmap :: (a -> b) -> AltF f a -> AltF f b
  fmap f (Pure a) = Pure $ f a
  fmap f (Ap x g) = x `Ap` fmap (f .) g

instance Functor (Alt f) where
  fmap :: (a -> b) -> Alt f a -> Alt f b
  fmap f (Alt xs) = Alt $ map (fmap f) xs

instance Applicative (AltF f) where
  pure :: a -> AltF f a
  pure = Pure
  
  (<*>) :: AltF f (a -> b) -> AltF f a -> AltF f b
  (Pure f)   <*> y         = fmap f y      -- fmap
  y          <*> (Pure a)  = fmap ($ a) y  -- interchange
  (Ap a f)   <*> b         = a `Ap` (flip <$> f <*> (Alt [b]))

instance Applicative (Alt f) where
  pure :: a -> Alt f a
  pure a = Alt [pure a]
  
  (<*>) :: Alt f (a -> b) -> Alt f a -> Alt f b
  (Alt xs) <*> ys = Alt (xs >>= alternatives . (`ap'` ys))
    where
      ap' :: AltF f (a -> b) -> Alt f a -> Alt f b
      Pure f `ap'` u      = fmap f u
      (u `Ap` f) `ap'` v  = Alt [u `Ap` (flip <$> f) <*> v]

instance Alternative (Alt f) where
  empty :: Alt f a
  empty = Alt []

  (<|>) :: Alt f a -> Alt f a -> Alt f a
  Alt as <|> Alt bs = Alt (as ++ bs)

liftAltF :: f a -> AltF f a
liftAltF x = x `Ap` pure id

liftAlt :: f a -> Alt f a
liftAlt = Alt . (:[]) . liftAltF

-- Character primitive
data Prim a = Prim Char a
  deriving Functor

type RegExp = Alt Prim

char' :: Char -> RegExp Char
char' c = liftAlt (Prim c c)

string' :: String -> RegExp String
string' = traverse char'

testRegExp :: RegExp Int
testRegExp = (char' 'a' <|> char' 'b')
          *> (length <$> many (string' "cd"))
          <* char' 'e'

{-
    Regex:                                            Free Alternative:
    c        - Parseol egy c karaktert                char' 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many' ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
-}

-- alm(a|ák)
p1' :: RegExp ()
p1' = 
  (string' "alm") *>
  (void $ char' 'a') <|> (void $ string' "ák") -- itt pl char :: F Char, string' :: F String

-- c(i+)ca
p2' :: RegExp ()
p2' =
  (char' 'c') *>
  (optional $ char' 'i') *>
  (void $ string' "ca")
