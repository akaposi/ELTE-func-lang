{-# LANGUAGE InstanceSigs #-}
module Practice9 where

import Control.Applicative
import Control.Monad (ap)
import Data.Char

-- Maybe, State, Writer

-- "2 + 3" -> Add (I 2) (I 3)
-- "2 +"   -> fail
-- "2 + 3 + 4" -> Add (I 2) (Add (I 3) (I 4))

newtype Parser1 a = P1 { runParser1 :: String -> Maybe (a, String) }
newtype Parser2 a = P2 { runParser2 :: String -> (Maybe a, String) }

char :: Char -> Parser2 Char
char x = P2 $ \str ->
  case str of
    c:cs | c == x -> (Just c, cs)
    _ -> (Nothing, str)

instance Functor Parser2 where
  fmap :: (a -> b) -> Parser2 a -> Parser2 b
  fmap f p = p >>= (\x -> return (f x))

-- instance Monad m => Applicative m where
--   pure :: a -> m a
--   pure = return

--   -- (>>=) :: m a -> (a -> m b) -> m b
--   (<*>) :: m (a -> b) -> m a -> m b
--   (<*>) mF mX = do
--     f <- mF
--     x <- mX
--     return $ f x

instance Applicative Parser2 where
  pure  = return
  (<*>) = ap

instance Monad Parser2 where
  return :: a -> Parser2 a
  return x = P2 $ \str -> (Just x, str)

  (>>=) :: Parser2 a -> (a -> Parser2 b) -> Parser2 b
  (>>=) p k = P2 $ \str ->
    case runParser2 p str of
      (Just x, str')  -> runParser2 (k x) str'
      (Nothing, str') -> (Nothing, str')

sequenceParser :: [Parser2 a] -> Parser2 [a]
sequenceParser [] = return []
sequenceParser (p:ps) = do
  a  <- p
  as <- sequenceParser ps
  return (a:as)

-- char   :: Char   -> Parser2 Char
-- string :: [Char] -> Parser2 [Char]
-- map char :: [Char] -> [Parser2 Char]

-- string :: [Char] -> Parser2 [Char]
string :: String -> Parser2 String
string str = sequenceParser $ map char str

-- runParser2 (char 'c') "c"
-- runParser2 (string "asd") "asd"

-- TODO: Parser that recognizes a character that satisfies a given predicate
-- NOTE: similarly to char
satisfy :: (Char -> Bool) -> Parser2 Char
satisfy pred = P2 $ \str ->
  case str of
    c:cs | pred c -> (Just c, cs)
    _ -> (Nothing, str)

-- NOTE: use satisfy, digitToInt, fmap
digit :: Parser2 Int
digit = digitToInt <$> satisfy (`elem` ['0'..'9'])

-- asd :: (a -> b) -> f a -> f b
asd :: (Char -> Int) -> Parser2 Char -> Parser2 Int
asd f p = do
  x <- p
  return $ f x

{- tests
runParser digit "45" == (Just 4, "5")
runParser digit "x5" == (Nothing, "5")
runParser digit "a5" == (Nothing, "5")
-}

-- foo :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d ???
-- fmap (,) digit :: Parser2 (b -> (Int,b))

digitCoordinate :: Parser2 (Int, Int)
digitCoordinate = (,) <$> digit <*> digit

digitVec3 :: Parser2 (Int, Int, Int)
-- (digit, digit, digit)
-- (,,) digit digit digit
-- (,,) <$> digit digit digit
-- (,,) <$> digit <*> digit <*> digit
digitVec3 = (,,) <$> digit <*> digit <*> digit

-- recognize N-long numbers
-- runParser2 (digitN 3) "123" == (Just [1,2,3], "")
-- runParser2 (digitN 3) "1234" == (Just [1,2,3], "4")
-- runParser2 (digitN 2) "12" == (Just [1,2,3], "4")
digitN :: Int -> Parser2 [Int]
digitN = undefined

digits :: Parser2 [Int]
digits = do
  n <- digit
  digitN n

combine :: Parser2 a -> Parser2 a -> Parser2 a
combine p q = P2 $ \str ->
  case runParser2 p str of
    (Just x, str')  -> (Just x, str')
    (Nothing, str') -> runParser2 q str

-- class Applicative f => Alternative f where
--   (<|>) :: f a -> f a -> f a

--   some :: f a -> f [a]
--   some f = (:) <$> f <*> many f

--   many :: f a -> f [a]
--   many f = some f <|> pure []

-- forall p. empty <|> p === p
-- forall p. p <|> empty === p

instance Alternative Parser2 where
  empty :: Parser2 a
  empty = P2 $ \str ->
    (Nothing, str)

  (<|>) = combine

true1 :: Parser2 Bool
true1 = do
  _ <- satisfy (== 't')
  _ <- satisfy (== 'r')
  _ <- satisfy (== 'u')
  _ <- satisfy (== 'e')
  return True

true2 :: Parser2 Bool
true2 = do
  _ <- char 't'
  _ <- char 'r'
  _ <- char 'u'
  _ <- char 'e'
  return True

true3 :: Parser2 Bool
true3 = do
  char 't'
  char 'r'
  char 'u'
  char 'e'
  return True

true4 :: Parser2 Bool
true4 = do
  string "true"
  return True

true :: Parser2 Bool
true = string "true" *> pure True

false :: Parser2 Bool
false = string "false" *> pure False

bool :: Parser2 Bool
bool = true <|> false
