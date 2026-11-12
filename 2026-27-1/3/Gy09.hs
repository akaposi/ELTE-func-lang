{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Use lambda-case" -}
module Gy09 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import Data.Traversable

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)} deriving (Functor)

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
    (c : cs) | p c -> Just (c, cs)
    _ -> Nothing

-- uncons input >>= \(c, cs) -> if p c then Just (c, cs) else Nothing

-- Olyan parser, amely akkor fogad el, ha nincs semmi a bemeneten
eof :: Parser ()
eof = Parser $ \input -> case input of
    [] -> Just ((), [])
    _ -> Nothing

-- Ezekből felépíthetőek egyéb parserek
-- Olyan parser, ami egy adott karaktert parseol
-- Itt irreleváns az, hogy mi a kimenet

-- >>> :t void
-- void :: Functor f => f a -> f ()

char :: Char -> Parser ()
char c =
    void $ satisfy (== c)

--        ()  <$  satisfy (== c)
-- (\_ -> ()) <$> satisfy (== c)

-- Parseoljunk akármilyen karaktert
anychar :: Parser Char
anychar = satisfy (const True) -- (\_ -> True)

-- Parseoljunk egy konkrét stringet
-- hint: mapM_
string :: String -> Parser ()
string [] = return ()
string (c : cs) = char c >> string cs

-- >>> runParser (mapM_ char "alma") "almafa"
-- Just ((),"fa")

-- >>> runParser (string "alma") "almafa"
-- Just ((),"fa")

-- Parsernél fontos a "vagy" művelet (ha az első parser elhasal, akkor a másikat próbáljuk meg)
-- Ez lesz az Alternative típusosztály
instance Alternative Parser where
    empty :: Parser a -- Garantáltan elhasaló parser
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a -- Ha a baloldali sikertelen, futassuk le a jobboldalit (hint: A Maybe is egy alternatív)
    Parser a <|> Parser b = Parser $ \input -> case a input of
        Just (x, xs) -> Just (x, xs)
        Nothing -> b input

-- Definiáljunk egy parsert ami egy 'a' vagy egy 'b' karaktert parseol

aorb :: Parser Char
aorb = ('a' <$ char 'a') <|> ('b' <$ char 'b')

-- >>> runParser aorb "balmafa"
-- Just ('b',"almafa")

-- many :: Parser a -> Parser [a]
-- 0 vagy többször lefuttatja a parsert
-- some :: Parser a -> Parser [a]
-- 1 vagy többször lefuttatja a parsert

many' :: Parser a -> Parser [a]
many' p = some' p <|> pure [] -- Ha 1 vagy több sikertelen, akkor 0 van

some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> many' p -- Lefuttatja 1x és utána 0 vagy többször

-- >>> runParser (some' aorb) "aaabababababababababbabacaabbabababab"
-- Just ("aaabababababababababbaba","caabbabababab")

-- optional' :: Parser a -> Parser (Maybe a)
-- ha elhasalna a parser, mégse hasal el
optional' :: Parser a -> Parser (Maybe a)
optional' p = Just <$> p <|> pure Nothing

-- >>> sequence [char c | c <- inputString]

-- replicateM :: Int -> Parser a -> Parser [a]
-- n-szer lefuttat egy parsert
replicateM' :: (Integral i) => i -> Parser a -> Parser [a]
replicateM' 0 a = return []
replicateM' n p = do
    par <- p
    par' <- replicateM' (n - 1) p
    return (par : par')

{-
replicateM 0 p = pure []
replicateM n p = (:) <$> p <*> replicateM (n - 1) p

replicateM n p = p >>= \c -> (c:) <$> replicateM (n - 1) p

replicateM n p = sequence [ p | _ <- [1..n] ]

-}

-- asum :: [Parser a] -> Parser a
-- Sorban megpróbálja az összes parsert lefuttatni
asum' :: [Parser a] -> Parser a
asum' [] = empty
asum' (x : xs) = x <|> asum' xs

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
    ℓ{n,}    - Parseol legalább n darab ℓ kifejezést  replicateM n ℓ >> many ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 digitToInt <$> satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  satisfy (\x -> x >= min c₁ c₂ && x <= max c₁ c₂)
    [c₁,c₂]  - c₁ vagy c₂ karaktert parseol           asum [char c₁, char c₂]
-}

-- Írjunk regex kifejezéseket parserekkel!

-- alm(a|ák)
p1 :: Parser ()
p1 = do
    string "alm"
    char 'a' <|> string "ák"

-- >>> runParser p1 "almá"
-- Nothing

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
    string "aaa"

-- (alma|banana)?
p4 :: Parser ()
p4 = void $ optional $ string "alma" <|> string "banana"

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

newtype NParser a = NParser {runNParser :: String -> [(a, String)]} deriving (Functor)

instance Applicative NParser where
    pure a = NParser $ \s -> [(a, s)]
    (<*>) = ap

instance Monad NParser where
    NParser a >>= f = NParser $ \s -> let p = a s in concatMap (\(a, r) -> runNParser (f a) r) p

charN :: Char -> NParser ()
charN c = NParser $ \input -> case input of
    [] -> []
    (k : ks) -> ([((), ks) | k == c])

stringN :: String -> NParser ()
stringN [] = pure ()
stringN (c : cs) = charN c >> stringN cs

anycharN :: NParser ()
anycharN = NParser $ \s -> case s of
    [] -> []
    (c : cs) -> [((), cs)]

instance Alternative NParser where
    empty = NParser $ const []
    NParser l <|> NParser r = NParser $ \s -> l s ++ r s

--- >>> runNParser (void $ many (charN 'a')) "aaaaaaab"
-- [((),"b"),((),"ab"),((),"aab"),((),"aaab"),((),"aaaab"),((),"aaaaab"),((),"aaaaaab"),((),"aaaaaaab")]
