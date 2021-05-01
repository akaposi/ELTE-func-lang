
{-# language DeriveFunctor #-}

import Control.Applicative
import Control.Monad

--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


--------------------------------------------------------------------------------

-- Hibakezelési modell:
--      - erős/gyenge hiba megkülönöztetése
--      - gyenge hiba: control-flow a célja           (backtrack-elhető)
--      - erős hiba:   csak hibás input esetén dobjuk (nem lehet belőle backtrack-elni)
--      - Betesszük a parser state-be a pozíció információkat
-- hackage: "flatparse"
-- Rust: "nom"

-- Alternatív hibakezelés: Parsec, Megaparsec library-k

type Pos = (Int, Int) -- (oszlop, sor)
  -- (lehet viszont csak String-ből visszaszámolni hiba nyomtatáskor (oszlop, sor) info-t)

data Result e a
  = OK a String Pos
  | Fail
  | Error e Pos
  deriving Functor

newtype Parser e a = Parser {runParser :: String -> Pos -> Result e a}
  deriving Functor

instance Applicative (Parser e) where
  pure = return
  (<*>) = ap

instance Monad (Parser e) where
  return a = Parser $ \s pos -> OK a s pos
  Parser f >>= g = Parser $ \s pos -> case f s pos of
    OK a s pos  -> runParser (g a) s pos
    Fail        -> Fail
    Error e pos -> Error e pos

eof :: Parser e ()
eof = Parser $ \s pos -> case s of
  [] -> OK () s pos
  _  -> Fail

satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = Parser $ \s (line, col) -> case s of
  c:s | f c -> case c of '\n' -> OK c s (line + 1, 0)
                         _    -> OK c s (line, col + 1)
  _         -> Fail

char c = satisfy (== c)

string :: String -> Parser e ()
string = mapM_ char

throw :: e -> Parser e a
throw e = Parser $ \s pos -> Error e pos

instance Alternative (Parser e) where
  empty = Parser $ \_ _ -> Fail
  Parser f <|> Parser g = Parser $ \s pos -> case f s pos of
    OK a s pos  -> OK a s pos
    Fail        -> g s pos
    Error e pos -> Error e pos

sepBy1 :: Parser e a -> Parser e b -> Parser e [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser e a -> Parser e b -> Parser e [a]
sepBy pa pb = sepBy1 pa pb <|> pure []
