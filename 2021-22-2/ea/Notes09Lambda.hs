
{-# language DeriveFunctor, InstanceSigs, BangPatterns #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Extra példa: lambdák kiértékelése

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char    -- isSpace, isDigit


-- Parser lib
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)   -- nincs hatás

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> do {(a, s) <- f s; runParser (g a) s}

-- pontosan az üres inputot olvassuk
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- olvassunk egy konkrét String-et
string :: String -> Parser ()   -- String ~ [Char]
string s = mapM_ char s         -- egymás után olvasom az összes Char-t a String-ben


instance Alternative Parser where
  -- mindig hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing -> g s
    res     -> res

-- Control.Applicative-ból:
--    many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
--    some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- Control.Applicative-ból:
-- optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
-- optional pa = (Just <$> pa) <|> pure Nothing

-- 0 vagy 1 eredményt olvasunk
optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)


-- token/ws parsing

ws :: Parser ()
ws = many_ (satisfy isSpace)

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- operátor segédfüggvények

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty


-- Lambda kalkulus interpreter
--------------------------------------------------------------------------------

data Tm
  = Var String          -- változó
  | Let String Tm Tm    -- let x = t1 in t2
  | Lam String Tm       -- \x. t
  | App Tm Tm           -- t1 t2
  | IntLit Int
  | Add Tm Tm           -- e1 + e2
  deriving (Eq, Show)

-- closure: - "captured env" lokális környezet a kiértékelés pontján
--          - függvény kód: Tm
data Val = VInt Int | VLam Env String Tm
  deriving (Eq, Show)

type Env = [(String, Val)]

eval :: Env -> Tm -> Val
eval env t = case t of
  Var x -> case lookup x env of
    Just v -> v
    _      -> error $ "name not in scope: " ++ x

  Let x t1 t2 ->
    let v = eval env t1 in eval ((x, v):env) t2

  Lam x t -> VLam env x t

  App t1 t2 -> case eval env t1 of
    VLam env' x t1 -> eval ((x, eval env t2):env') t1
    _              -> error "type error"

  IntLit n -> VInt n

  Add e1 e2 -> case (eval env e1, eval env e2) of
    (VInt n1, VInt n2) -> VInt (n1 + n2)
    _                  -> error "type error"

--------------------------------------------------------------------------------

keywords :: [String]
keywords = ["let", "in"]

ident' :: Parser String
ident' = do
  x <- some (satisfy isAlpha) <* ws
  if elem x keywords
    then empty
    else pure x

keyword' :: String -> Parser ()
keyword' str = do
  x <- some (satisfy isAlpha) <* ws
  if x == str
    then pure ()
    else empty

posInt' :: Parser Int
posInt' = do
  digits <- some (satisfy isDigit)
  ws
  pure (read digits)

atom :: Parser Tm
atom =   (IntLit <$> posInt')
    <|>  (char' '(' *> tm <* char' ')')
    <|>  (Var <$> ident')

app :: Parser Tm
app = leftAssoc App atom (pure ())

add :: Parser Tm
add = rightAssoc Add app (char' '+')

letLam :: Parser Tm
letLam =
      (Let <$> (keyword' "let" *> ident' <* string' "=")
           <*> tm
           <*> (keyword' "in" *> letLam))
  <|> (Lam <$> ((char' '\\' <|> char' 'λ') *> ident' <* char' '.')
           <*> letLam)
  <|> add

tm :: Parser Tm
tm = letLam

run :: String -> Val
run str = case runParser (topLevel tm) str of
  Just (t, _) -> eval [] t
  _           -> error "parser error"

p1 = "λ x. x"
p2 = "(λ x. x) 10"

p3 = unlines [
  "let id = λ x. x in",
  "let const = λ x. λ y. x in",
  "let f = λ x. λ y. x + x + y in",
  "let g = λ x. x + 100 in",
  "let comp = λ f. λ g. λ x. f (g x) in",
  "comp g (comp g g) 0"
  ]

p4 = unlines [
  "let x = 10 in",
  "let z = 10 in",
  "let a = 10 in",
  "let f = λ y. y + x in",   -- capture "x", "z", "a"  (csak az "x"-et kéne elmenteni!)
  "f"
  ]

-- optimalizáció: minden lambda kizárólag a használt változókat menti el
--   (első osztályú függvények más nyelvekben így működnek)
