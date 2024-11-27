{-# LANGUAGE LambdaCase #-}
module Email where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Data.Char
import Data.List
import Data.Bifunctor
import Control.Monad

type Parser a = StateT String (Except ()) a

runParser :: Parser a -> String -> Maybe (a, String)
runParser p s = (\case Left _ -> Nothing; Right a -> Just a) $ runExcept (runStateT p s)

parseError :: String -> Parser a
parseError _ = throwError ()

-- Primitívek

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> parseError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> parseError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> parseError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> parseError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> parseError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

many' :: Parser a -> Parser [a]
many' = many -- Ha 1 vagy több sikertelen, akkor 0 van

some' :: Parser a -> Parser [a]
some'= some

optional' :: Parser a -> Parser (Maybe a)
optional' = optional

-- Regex féle parserek
{-
    Regex:                                            Haskell megfelelő:
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

-- mail = ([A-Z]|[a-z]|[0-9])+(\+([A-Z]|[a-z]|[0-9])+)?@([A-Z]|[a-z]|[0-9])+(\.([A-Z]|[a-z]|[0-9])+)+$
mail :: Parser ()
mail = let 
  chars = some (satisfy (\x -> x >= min 'A' 'Z' && x <= max 'A' 'Z') <|> satisfy (\x -> x >= min 'a' 'z' && x <= max 'a' 'z') <|> satisfy (\x -> x >= min '0' '9' && x <= max '0' '9'))
  in chars *> optional (char '+' *> chars) *> char '@' *> chars *> some (char '.' *> chars) *> eof

{-

>>> runParser mail "blah" == Nothing
True

>>> runParser mail "fcjylp+atInfDotElteDotHu" == Nothing
True

>>> runParser mail "fcjylp+@inf.elte.hu" == Nothing
True

>>> runParser mail "fcjylp+@infeltehu" == Nothing
True

>>> runParser mail "fcjylp+test@inf.elte.hu" == Just ((), "")
True

>>> runParser mail "fcjylp@inf.elte.hu" == Just ((), "")
True

>>> runParser mail "fcjylp+test@elte.hu" == Just ((), "")
True

>>> runParser mail "fcjylp@elte.hu" == Just ((), "")
True

-}
