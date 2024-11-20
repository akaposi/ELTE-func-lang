{-# LANGUAGE LambdaCase #-}
module Parse where


import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.List
import Data.Bifunctor
import Control.Monad.Cont

-- Parser hibaüzenettel
type Parser  = StateT String (Except String)

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

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

braces :: Parser ()
braces = do
  c <- braces'
  if c == 0 then do
    pure ()
  else
    throwError "BRACES MISSMATCH"
    where
      braces' :: Parser Integer
      braces' = do
        s <- get
        if null s then do
          pure 0
        else do
            i <- char '(' *> pure 1 <|> char ')' *> pure (-1) <|> (fmap (drop 1) get >>= put) *> pure 0
            r <- braces'
            pure $ r + i
        
-- >>> (runParser braces "()") == Right ((),"")
-- True
-- >>> (runParser braces "abcd(abcd)abcd") == Right ((),"")
-- True
-- >>> (runParser braces $ replicate 100 '(' ++ replicate 100 ')') == Right ((),"")
-- True
-- >>> (runParser braces $ replicate 100 '(' ++ replicate 100 ')' ++ ")(") == Right ((),"")
-- True
-- >>> (runParser braces $ replicate 100 '(' ++ replicate 100 ')' ++ ")") == Left "BRACES MISSMATCH"
-- True

