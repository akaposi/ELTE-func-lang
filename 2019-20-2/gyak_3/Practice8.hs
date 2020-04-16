{-# LANGUAGE InstanceSigs #-}
module Practice8 where

import Control.Monad (ap)

-- Maybe, State, Writer

-- "2 + 3" -> Add (I 2) (I 3)
-- "2 +"   -> fail
-- "2 + 3 + 4" -> Add (I 2) (Add (I 3) (I 4))

newtype Parser1 a = P1 { runParser1 :: String -> Maybe (a, String) }
newtype Parser2 a = P2 { runParser2 :: String -> (Maybe a, String)   }

char :: Char -> Parser2 Char
char x = P2 $ \str ->
  case str of
    c:cs | c == x -> (Just c, cs)
    _ -> (Nothing, str)

instance Functor Parser2 where
  fmap :: (a -> b) -> Parser2 a -> Parser2 b
  fmap f p = p >>= (\x -> return (f x))

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
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = undefined

-- NOTE: use satisfy, digitToInt, fmap
digit :: Parser2 Int
digit = undefined

{- tests
runParser digit "45" == (Just 4, "5")
runParser digit "x5" == (Nothing, "5")
runParser digit "a5" == (Nothing, "5")
-}

digitCoordinate :: Parser2 (Int, Int)
digitCoordinate = undefined

-- recognize N-long numbers
-- runParser2 (digitN 3) "123" == (Just [1,2,3], "")
-- runParser2 (digitN 3) "1234" == (Just [1,2,3], "4")
-- runParser2 (digitN 2) "12" == (Just [1,2,3], "4")
digitN :: Int -> Parser2 [Int]
digitN = undefined
