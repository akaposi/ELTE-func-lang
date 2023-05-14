
import Control.Monad
import Control.Applicative
import Data.Char
import Debug.Trace (trace, traceShow, traceM, traceShowM)

-- Parser folyt
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where

  -- return: nincs mellékhatás
  return :: a -> Parser a
  return = pure

  -- egymást után két parser-t végrehajtunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser f) g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

-- parser, ami üres inputot fogadja csak el
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- sikeres: az input első karakterére igaz egy feltétel,
-- kiolvassuk és visszaadjuk
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- konkrét karaktert olvasunk
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

instance Alternative Parser where
  -- rögtön hibázó parser
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- ha pa hibázik, futtassuk pb-t
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String-et olvassunk
string :: String -> Parser ()
string s = mapM_ char s

-- standard függvények: Control.Applicative-ból
-- many, some

many_ :: Parser a -> Parser ()
many_ pa = some_ pa <|> pure ()

some_ :: Parser a -> Parser ()
some_ pa = pa >> many_ pa

posInt :: Parser Int
posInt = read <$> some (satisfy isDigit)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s ->
  trace (msg ++ " : " ++ s) (runParser pa s)

-- Precedencia segédfüggvények
------------------------------------------------------------

rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

-- nem asszociatív:
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

-- nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa

-- parser, ami megeszi a whitespace-eket
ws :: Parser ()
ws = many_ (satisfy isSpace)

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

-- vesszős parserek kezelik a ws-t
char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

posInt' :: Parser Int
posInt' = posInt <* ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws


-- Típusozatlan lambda kalkulus
------------------------------------------------------------

data Tm = Var String | App Tm Tm | Lam String Tm
  deriving Show


-- Var "x"                              x
-- App (Var "f") (Var "x")              f x
-- Lam "x" (Var "x")                    λ x. x

-- foo f x y z = ((f x) y) z

-- kötési erősségek:
--      1. literál, zárójelezett kifejezés
--      2. App:           balra asszociáló infix operátor (whitespace)
--      3. Let, Lambda:   prefix, láncolható

pIdent :: Parser String
pIdent = do
  x <- some (satisfy (\c -> isLetter c || isDigit c)) <* ws
  if x == "let" then empty else pure x

pAtom  :: Parser Tm
pAtom = (Var <$> pIdent)
    <|> (char' '(' *> pLam <* char' ')')

pApp :: Parser Tm
pApp = leftAssoc App pAtom (pure ())

buildLet :: String -> Tm -> Tm -> Tm
buildLet x t u = App (Lam x u) t

pLam :: Parser Tm
pLam = (Lam <$> (char' 'λ' *> pIdent <* char' '.')
            <*> pLam)                              -- láncolható prefix operátor
   <|> (buildLet <$> (string' "let" *> pIdent <* string' "=")
                 <*> (pLam <* char' ';')
                 <*> pLam)
   <|> pApp

pTm :: Parser Tm
pTm = topLevel pLam

------------------------------------------------------------

-- str1 ++ str2
-- (\s -> str1 ++ s) . (\s -> str2 ++ s)

pretty :: Tm -> String
pretty t = go 2 t "" where

  patom = 0 :: Int
  papp  = 1 :: Int
  plam  = 2 :: Int

  par :: Int -> Int -> (String -> String) -> (String -> String)
  par p1 p2 str | p2 > p1   = ('(':) . str . (')':)
                | otherwise = str

  go :: Int -> Tm -> String -> String
  go prec t = case t of
    Var x   -> (x++)
    App t u -> par prec papp (go papp t . (' ':) . go patom u)
    Lam x t -> par prec plam (("λ "++) . (x++) . (". "++) . go plam t)

t1 = App (Lam "x" (Lam "y" (Var "y"))) (Var "f")

-- Interpreter
------------------------------------------------------------

-- Ha nincs primitív adat, csak függvény:
--     Kiértékelés: "normalizáció"
--        (λ x. x) (λ x. x)            ~>     λ x. x
--        (λ x. ((λ x. x) (λ x. x)))   ~>     λ x y. y

-- foo = \x -> ((\x -> x) (\x -> x))

--  (Irány 1: csak lambda van, és kifejezés normalizálás
--            csak lambdából minden adatstruktúrát definiálni tudunk)

--   λ x. ((λ y. y) x)     ~>      λ x. x

-- (Zárt vs. nyílt kiértékelés
--    zárt:   minden változónak van értéke (standard FP)
--    nyílt:  "szimbólikus", bizonyos változóknak nincsen értéke

-- (Irány 2: legyen primitív adattípus
--           elég csak a szabad változó nélküli kifejezéseket interpretálni
--           (standard implementáció FP-ban)

-- 1 vs 2?

-- Nyílt kiértékelés

type Env = [(String, Val)] -- "environment"  (névhez Val-t rendel)

data Val = VVar String | VApp Val Val | VLam String Env Tm
  deriving Show

eval :: Env -> Tm -> Val
eval env t = case t of
  Var x   -> case lookup x env of
               Nothing -> error $ "name not in scope: " ++ x
               Just v  -> v
  Lam x t -> VLam x env t
  App t u -> case eval env t of
               VLam x env' t' -> eval ((x, eval env u):env') t'
               t              -> VApp t (eval env u)

fresh :: [String] -> String -> String
fresh xs x = if elem x xs then fresh xs (x++"'")
                          else x

-- "fordított" kiértékelés, kifejezést csinál value-ból
quote :: [String] -> Val -> Tm
quote xs v = case v of
  VVar x       -> Var x
  VApp t u     -> App (quote xs t) (quote xs u)
  VLam x env t -> let x' = fresh xs x
                  in Lam x' (quote (x':xs) (eval ((x, VVar x'):env) t))

run :: String -> IO ()
run path = do
  str <- readFile path
  case runParser pTm str of
    Nothing     -> error "parse error"
    Just (t, _) -> print t
      -- putStrLn $ pretty (quote [] (eval [] t))
