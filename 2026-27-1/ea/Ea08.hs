module Ea08 where

import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char

import Control.Concurrent
import Control.Concurrent.Async -- need the `async` library

-- (<*>) :: f (a -> b) -> f a -> f b
-- (=<<) :: (a -> f b) -> f a -> f b

-- (>>=) :: f a -> (a -> f b) -> f b

newtype Reverse f a = Reverse (f a)

runReverse :: Reverse f a -> f a
runReverse (Reverse fa) = fa

instance Functor f => Functor (Reverse f) where
  fmap g (Reverse fa) = Reverse $ fmap g fa

instance Applicative f => Applicative (Reverse f) where
  pure a = Reverse $ pure a
  Reverse fg <*> Reverse fa = Reverse $ liftA2 (\a g -> g a) fa fg

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c


revCumSum :: Traversable t => t Int -> t Int
revCumSum t =
  fst $
    runState
      (runReverse $
        traverse
          (\n -> Reverse $ do
            s <- get
            let s' = s + n
            put s'
            return s')
          t)
      0



example :: IO ()
example = do
  threadDelay (3 * 1000000)
  putStrLn "hello"


delayN :: Int -> IO ()
delayN n = do
  threadDelay (n * 1000000)
  putStrLn $ "delayed for " ++ show n ++ " seconds"


example2 :: IO ()
example2 =
  runConcurrently $
    traverse_ (\n -> Concurrently $ delayN n) [1..5]

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse_ :: Applicative f => (a -> f b) -> t a -> f () -- in Data.Foldable








newtype Parser a = Parser (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser p) = p

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser p >>= f = Parser $ \s -> case p s of
    Nothing -> Nothing
    Just (a, s') -> runParser (f a) s'

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
-}

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p <|> Parser q = Parser $ \s -> case p s of
    Nothing -> q s
    Just (a, s') -> Just (a, s')


{-
optional :: Alternative f => f a -> f (Maybe a)
many :: Alternative f => f a -> f [a]
some :: Alternative f => f a -> f [a]
-}

{-
Haskell        Regex
a <*> b         ab
a <|> b         a|b
pure            -- empty string
empty           -- always fails
optional a      a?
many a          a*
some a          a+
-}



char :: Char -> Parser ()
char c = void $ satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  "" -> Nothing
  c:rest
    | p c -> Just (c, rest)
    | otherwise -> Nothing

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((), s) else Nothing

-- Applicative f => f a -> f b -> f b

{-

-- a? = (a|)
optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
-- p :: f a
-- need f (Maybe a)
-- Just :: a -> Maybe a

-- a* = (a+|)
many :: Alternative f => f a -> f [a]
many p = some p <|> pure []

-- a+ = aa*
some :: Alternative f => f a -> f [a]
some p = liftA2 (:) p (many p)
-- p :: f a
-- many p :: f [a]
-}


data Tree = Leaf | Node Tree Tree
  deriving (Show)

-- .
-- (..)
-- (.((..).))
-- ((..)(..))

parseTree :: Parser Tree
parseTree =
  (do
    char '.'
    return Leaf) <|>
  (do
    char '('
    l <- parseTree
    r <- parseTree
    char ')'
    return $ Node l r
  )

parseTree' :: Parser Tree
parseTree' =
  Leaf <$ char '.' <|>
  Node <$ char '(' <*> parseTree' <*> parseTree' <* char ')'

-- a <$ p = const a <$> p
-- <*

digitsToInt :: [Char] -> Int
digitsToInt s = foldl (\n d -> 10 * n + d) 0 $ map digitToInt s

parseNat :: Parser Int
parseNat = digitsToInt <$> some (satisfy isDigit)




example3 :: Parser ()
example3 = do
  n <- parseNat
  replicateM n (char 'a')
  return ()
