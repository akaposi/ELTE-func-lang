{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Foldable

--- PARSER

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \st -> case p st of
        Nothing -> Nothing
        Just (st', a) -> Just (st', f a)

instance Applicative Parser where
    pure a = Parser $ \st -> Just (st, a)
    liftA2 f (Parser p1) (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> case p2 st' of
            Nothing -> Nothing
            Just (st'', b) -> Just (st'', f a b)

instance Monad Parser where
    (Parser p1) >>= f = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> runParser (f a) st'

instance Alternative Parser where
    empty = Parser $ \st -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> p2 st
        Just (st', a) -> Just (st', a)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (xs, x) else Nothing

char :: Char -> Parser ()
char c = void $ satisfy (\c' -> c' == c) -- satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (\x -> True)

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ([], ())
    _ -> Nothing

--- PARSER VÉGE

data DoubleList a = Cons a a (DoubleList a) | Nil deriving (Functor, Foldable)

instance Traversable DoubleList where
    sequenceA (Cons a a' dl) = Cons <$> a <*> a' <*> sequenceA dl -- vagy liftA3 Cons a a' (sequenceA dl)
    sequenceA Nil            = pure Nil

threeOrMoreCs :: Parser ()
threeOrMoreCs = void $ replicateM 3 (char 'c') *> many (char 'c')
 

-- Definiáljuk az alábbi parsert.
-- pl choice [char 'c', string "a", char 'q'] == char 'c' <|> string "a" <|> char 'q' <|> empty
choice :: [Parser a] -> Parser a
choice [] = empty
choice (x:xs) = x <|> choice xs

-- Létező függvény: asum
{-
:t asum
asum :: (Foldable t, Alternative f) => t (f a) -> f a
-}

asum' :: (Foldable t, Alternative f) => t (f a) -> f a
asum' = foldr (<|>) empty


{-
    Regex gyorstalpaló:                                     Haskell megfelelő:
    c          - Parseol egy c karaktert                    char 'c'
    ℓ₁ℓ₂       - Parseol ℓ₁-et majd ℓ₂-t                    ℓ₁ *> ℓ₂ vagy ℓ₁ <* ℓ₂
    ℓ+         - Parseol 1 vagy több ℓ kifejezést           some ℓ
    ℓ*         - Parseol 0 vagy több ℓ kifejezést           many ℓ
    (ℓ₁|ℓ₂)    - Parseol ℓ₁-t vagy ℓ₂-t                     ℓ₁ <|> ℓ₂
    ℓ?         - Parseol 0 vagy 1 ℓ kifejezést              optional ℓ
    .          - Akármilyen karakter                        anyChar
    ℓ{n}       - Parseol n darab ℓ kifejezést               replicateM n ℓ
    $          - Nincs mit parseolni                        eof
    \d         - Parseol egy számjegyet                     satisfy isDigit
    [c₁c₂⋅⋅ cₙ] - Parseolja valamelyiket a karakterek közül  asum $ (\c -> c <$ char c) <$> [c₁,c₂⋅⋅ cₙ] 
    [c₁-c₂]    - c₁ és c₂ között parseol egy karaktert      asum $ (\c -> c <$ char c) <$> [c₁..c₂]
    ℓ{k,}      - legalább k-t parseol ℓ-ből (ℓ{k}ℓ*)        replicateM k ℓ *> many ℓ                 
-}

-- Hasznos oldal: https://regexr.com/

-- Emlékeztető: mivel a parser monád, lehet do notációt használni
{-
char 'c' *> char 'a' *> char 'r'
≡
do
    char 'c'
    char 'a'
    char 'r'
-}

-- Írjuk meg az alábbi regex-et parserként

string :: [Char] -> Parser ()
string [] = pure ()
string (x:xs) = char x >> string xs

-- (alma|banana)?
p1 :: Parser ()
p1 = do
    optional $ string "alma" <|> string "banana"
    return ()

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = oneOf [c1..c2]
    where
        oneOf [] = empty
        oneOf (x:xs) = (x <$ char x) <|> oneOf xs

-- [A-Z]{10}
p2 :: Parser ()
p2 = void $ replicateM 10 $ asum $ (\c -> c <$ char c) <$> ['A' .. 'Z']

-- \d{2}
p3 :: Parser ()
p3 = void $ replicateM 2 $ digit

-- \d+.*$
p4 :: Parser ()
p4 = void $ some (satisfy isDigit) *>  many anyChar *> eof

-- \d?alma.?banana
p5 :: Parser ()
p5 = do
    optional $ satisfy isDigit
    string "alma"
    optional $ anyChar
    string "banana"
    return ()

-- (ab)?ba+.*
p6 :: Parser ()
p6 = do
    optional $ string "ab"
    char 'b'
    some $ char 'a'
    many $ anyChar
    return ()

-- \d{2,}-?$
p7 :: Parser ()
p7 = void $ replicateM 2 (satisfy isDigit) *> many (satisfy isDigit) *> optional (char '-') *> eof

-- [A-Z]*[1,2,3,9]?(A|Z)$
p8 :: Parser ()
p8 = do
    many $ asum $ (\c -> c <$ char c) <$> ['A'..'Z']
    optional $ asum $ (\c -> c <$ char c) <$> ['1', '2', '3', '9']
    char 'A' <|> char 'Z'
    eof

-- a+b*c?d{4}e{5,}
p9 :: Parser ()
p9 = undefined

-- alm(a|ák)|banán(ok)?
p10 :: Parser ()
p10 = undefined

-- Nem-regex parserek

-- Definiáljuk a bewteen parsert ami parseol egy kifejezést két kifejezés között
-- between (string "kecske") (string "alma") (char ')') kéne parsolenia azt hogy "kecskealma)"
between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = do
    l
    a' <- a
    r
    return a'
-- between l a r = l *> a <* r

-- Definiáljuk a sepBy és a sepBy1 parsereket, amik legalább 0 vagy legalább 1 elemet parseolnak elválasztva valami kifejezéssel!

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy pa pdelim = sepBy1 pa pdelim <|> pure []

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 pa pdelim = do
    a <- pa
    delim <- optional $ pdelim
    case delim of
        Just _ -> (\as -> a : as) <$> sepBy1 pa pdelim
        Nothing -> return [a]


-- Definiáljunk egy parsert ami egy listányi kifejezést parseol
-- pl.: runParser (listOf (satisfy isDigit)) "[1,2,3,4]" == Just ("", ['1','2','3','4']) 

listOf :: Parser a -> Parser [a]
listOf p = between (char '[') (sepBy p (char ',')) (char ']')

digit :: Parser Int
digit = (\s -> read [s]) <$> satisfy isDigit

-- Definniáljuk egy pozitív, majd előjeles integer parsert!

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> some digit

integer :: Parser Int
integer = do
    l <- optional $ char '-'
    case l of
        Just _ -> negate <$> natural
        _      -> natural

-- Float parser, nem kell tudni
float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    return $ s * (r / 10  + fromIntegral i)

-- Definiáljuk a balancedBracket parsert ami egyensúlyozott zárójeleket parseol
-- pl (())()(()()) egyensúlyozott
-- pl (()          nem

balancedBracket :: Parser ()
balancedBracket = undefined

-- Tokenizálás
-- ws parser: megeszi az összes space-t
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- tok parser: egy parser után megeszi az összes space-t
tok :: Parser a -> Parser a
tok p = p <* ws

{-
ha tok-olt parsereket használunk mindig akkor igazából egy helyen maradhat space: az elején
topLevel parser: az elejéről megeszi a whitespaceket
-}

topLevel :: Parser a -> Parser a
topLevel p = ws *> p


-- Definiáljunk néhány előbbi parsert hogy a felesleges space-k ne zavarják
-- runParser listOfDigits " [      1, 2       ,3,3        ]   " == Just ("", [1,2,3,3])
listOfDigits :: Parser [Int]
listOfDigits = topLevel listOfDigits'
    where
        listOfDigits' = tok $ between (tok $ char '[') (tok $ sepBy digit (tok $ char ',')) (tok $ char ']')

balancedBracket' :: Parser ()
balancedBracket' = undefined