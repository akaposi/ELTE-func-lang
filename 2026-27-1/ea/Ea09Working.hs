{- HLINT ignore "Use lambda-case" -}
module Ea09 where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Debug.Trace

newtype Parser a = Parser {runParser :: (String -> Maybe (a, String))} deriving (Functor)

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (<*>) = ap

instance Monad Parser where
    Parser a >>= f = Parser $ \s -> case a s of
        Just (a', s') -> runParser (f a') s'
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser a <|> Parser b = Parser $ \s -> a s <|> b s

-- for educational purposes
-- (c*)(i?)(c+)a{3}

-- mivel a parserunk um "greedy" ezért, a c* lenyeli az összes lehetséges c-t és ha nincs i akkor a c+ garantáltan elhasal
-- csináljuk meg, hogy a haskell ne különözzön

newtype NParser a = NParser {runNParser :: String -> [(a, String)]} deriving (Functor)

instance Applicative NParser where
    pure a = NParser $ \s -> [(a, s)]
    (<*>) = ap

instance Monad NParser where
    NParser a >>= f = NParser $ \s -> a s >>= \(b, s') -> runNParser (f b) s'

instance Alternative NParser where
    empty = NParser $ const []
    NParser a <|> NParser b = NParser $ \s -> a s <|> b s

satisfy :: (Char -> Bool) -> NParser Char
satisfy p = NParser $ \s -> case s of
    (c : cs) | p c -> [(c, cs)]
    _ -> []

anyChar :: NParser Char
anyChar = satisfy (const True)

char :: Char -> NParser ()
char c = void $ satisfy (== c)

eof :: NParser ()
eof = NParser $ \s -> case s of
    [] -> [((), [])]
    _ -> []

-- >>> :t some
-- some :: Alternative f => f a -> f [a]

-- >>> :t optional
-- optional :: Alternative f => f a -> f (Maybe a)

cica :: NParser String
cica = do
    many (satisfy (== 'c'))
    optional (satisfy (== 'i'))
    some (satisfy (== 'c'))
    replicateM_ 3 (satisfy (== 'a'))
    return "SIKERESEN CICÁT PARSEOLTUNK"

-- >>> runNParser cica "caaa"
-- [("SIKERESEN CIC\193T PARSEOLTUNK","")]

-- Do not use
uhoh :: NParser ()
uhoh = do
    many anyChar
    many anyChar
    many anyChar
    many anyChar
    return ()

-- Except(T)
-- State(T)

type AParser a = StateT String (Except String) a

runAParser p s = runExcept (runStateT p s)

(<||>) :: AParser a -> AParser a -> AParser a
f <||> g = f `catchError` const g

many' p = some' p <||> pure []
some' p = (:) <$> p <*> many' p

satisfy' :: (Char -> Bool) -> AParser Char
satisfy' p =
    get >>= \s -> case s of
        (c : cs) | p c -> c <$ put cs
        _ -> throwError "Hiba"

char' :: Char -> AParser ()
char' c = void (satisfy' (== c))

natural :: AParser Integer
natural = sum . zipWith (\i v -> fromIntegral v * 10 ^ i) [0 ..] . reverse <$> some' (digitToInt <$> satisfy' isDigit)

-- >>> runAParser natural "900000000000000000000000000000000000000000000000000000000000000000000000"
-- Right (900000000000000000000000000000000000000000000000000000000000000000000000,"")

optionalA p = (Just <$> p) <||> pure Nothing

integer :: AParser Integer
integer = do
    f <- maybe id (const negate) <$> optionalA (char' '-')
    f <$> natural

-- >>> runAParser integer "-123"
-- Right (-123,"")

float :: AParser Double
float = do
    base <- integer
    i <- optionalA (char' '.')
    case i of
        Nothing -> return (fromIntegral base)
        Just _ -> do
            zeroes <- length <$> many' (satisfy' (== '0'))
            tort <- natural
            -- 123 -> 0.123
            let p = fromIntegral tort / (10 ** (fromIntegral zeroes + fromIntegral (ceiling (logBase 10 (fromIntegral tort)))))
            -- traceShowM (fromIntegral tort)
            -- traceShowM (fromIntegral zeroes)
            -- traceShowM (logBase 10 (fromIntegral tort))
            return (fromIntegral base + p)

-- >>> runAParser float "1.5"
-- Right (1.5,"")

sepBy :: AParser a -> AParser delim -> AParser [a]
sepBy a delim = sepBy1 a delim <||> pure []

-- a | ,b,c,d,e,f,g,h
sepBy1 :: AParser a -> AParser delim -> AParser [a]
sepBy1 a delim = do
    a' <- a
    as <- many' (delim *> a)
    return (a' : as)

listLiteral :: AParser [Integer]
listLiteral = do
    char' '['
    a <- sepBy integer (char' ',')
    char' ']'
    return a

-- >>> runAParser listLiteral "[1,2,3,-4,352635267352675321352176357216531 ]"
-- Left "Hiba"

ws :: AParser ()
ws = void $ many $ satisfy' isSpace

tok :: AParser a -> AParser a
tok a = a <* ws

char'' :: Char -> AParser ()
char'' c = tok $ char' c

integer' :: AParser Integer
integer' = tok integer

goodlistLiteral :: AParser [Integer]
goodlistLiteral = do
    char'' '['
    a <- sepBy integer' (char'' ',')
    char'' ']'
    return a

-- >>> runAParser goodlistLiteral "[     1,                                     2                       , 3 ]            "
-- Right ([1,2,3],"")

-- 1 + 2 * 3 - 4
-- polish notation: Shunting-Yard algoritmus
-- ezen a tárgyon: Recursive Descent Parsing
