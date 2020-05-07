{-# language DeriveFunctor #-}

import Prelude hiding (exp)
import Control.Monad
import Control.Applicative    -- many, some
import Data.Char -- isSpace, isLetter, isDigit, isAlpha

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

--------------------------------------------------------------------------------

-- lambda kalkulus parse, kiértékelés closure-el, esetleg Reader-el

-- típusozatlan lambda kalkulus + let kifejezés
--   (let kifejezés emulálható: let x = t in u   ~   (\x -> u) t)
--   (mégis kezeljük, részben parser gyakorlatként, de: bonyolultabb nyelvekben
--     gyakran *nem* reprezentálható a let lambdával)

-- "term"
data Tm =
    Var String        -- x, y, z         (változónév: isLetter-ből nemüres String)
  | App Tm Tm         -- t u             (szóköz bal-asszociatív operátorként viselkedik)
  | Lam String Tm     -- lam x. t
  | Let String Tm Tm  -- let x = t in u
  | IntLit Int
  | Add Tm Tm         -- t + u
  deriving Show

t1 :: Tm
t1 = Let "f" (Lam "x" $ Var "x") $ App (Var "f") (Var "f")
-- let f = lam x. x in f f

-- parser
------------------------------------------------------------

string' :: String -> Parser ()
string' str = string str <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

-- "parse identifier"
-- pIdent :: Parser String
-- pIdent = some (satisfy isLetter)
  -- kettő dolog hiányzik innen:
  --   nem olvas ws-t
  --   nem kezeli a keyword-öket

keyword :: [String]             -- ident *nem* lehet keyword!
keyword = ["let", "lam", "in"]

pIdent :: Parser String
pIdent = do
  str <- some (satisfy isLetter) <* ws
  if elem str keyword
    then empty
    else pure str

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pAtom :: Parser Tm
pAtom = (Var <$> pIdent)                 -- azonosító
    <|> (char' '(' *> pTm <* char' ')')  -- zárójelezett kifejezés
    <|> (IntLit <$> pIntLit)

pLet :: Parser Tm   -- let x = t in u
pLet = do
  string' "let"
  x <- pIdent
  char' '='
  t <- pTm
  string' "in"
  u <- pTm
  pure $ Let x t u

pLam :: Parser Tm
pLam = do
  string' "lam"
  x <- pIdent
  char' '.'
  t <- pTm
  pure $ Lam x t

-- 1 vagy több applikáció olvasása
-- (infix operátor-nál egyszere n-darab alkalmazást érdemes olvasni)
-- (some/many használata után foldl/foldr-el az eredmény összerakni)
pApps :: Parser Tm
pApps = foldl1 App <$> some pAtom     -- szóköz infix operátor!

-- 1 vagy több + alkalmazás
pAdds :: Parser Tm
pAdds = foldl1 Add <$> sepBy1 pApps (char' '+')

pTm :: Parser Tm
pTm = pLet <|> pLam <|> pAdds

pSrc :: Parser Tm
pSrc = ws *> pTm <* eof   -- "standard" top-level parser


-- Kiértélés
--------------------------------------------------------------------------------

-- Csak zárt kifejezések kiértékelése
--   zárt kifejezés: nem szerepel benne szabad változónév
--   pl: let f = lam x. x + x + x in f 100
--   ennek eredménye: 300

--   nyílt kifejezés:
--   (lam x. x) y
--   eredmény: y

-- Hagyományos prognyelvek: csak zárt programokat értékelünk ki,
--   minden névnek van valahol definíciója

-- Nyílt kiértékelés: fontos, ha valamilyen számítás/transzormációt szeretnénk
--   elvégezni, akár függvények belsejében is

-- pl: lam x. x + 0 + 0     (szeretnénk optimalizálni)
--   lambda alatt az "x" szabad változó!
--   fordítóprogramnak + típusellenőrzőnek tudnia kell interpretálni szabad változókkal
--     együtt is!

-- 1. zárt kif. interpeter
------------------------------------------------------------

-- Mi a runtime object?


data Val
  = VIntLit Int      -- Int objektum

  | VLam String          -- lam kötött változó
        [(String, Val)]  -- "capture", változó-érték hozzárendelés
        Tm               -- függvény test ("függvény kód")
  deriving Show

eval :: [(String, Val)] -> Tm -> Val
eval env t = case t of
  Var x     -> case lookup x env of
                 Nothing -> error ("variable not in scope: " ++ x)
                 Just v  -> v
  App t u   -> case eval env t of
                 VLam x env' t -> eval ((x, eval env u):env') t
                 _             -> error "expected function in application"
  Lam x t   -> VLam x env t
  Let x t u -> eval ((x, eval env t):env) u
  IntLit n  -> VIntLit n
  Add t u   -> case (eval env t, eval env u) of
                 (VIntLit n, VIntLit n') -> VIntLit (n + n')
                 _                       -> error "expected integers in + operands"

-- Házi feladat:
-- p1 = "let f = x + 10 in f" nem parse-ol, kideríteni, hogy miért?


-- példa: magasabbrendű függvény is használható!
p1 :: String
p1 = "let f = (lam x. x + x + x + 10) in " ++
     "let twice = (lam f. lam x. f (f x)) in " ++
     "let comp = (lam f. lam g. lam x. f (g x)) in " ++
     "(comp (twice f) (twice f)) 100"   -- f (f (f (f 100)))

run :: String -> Val
run str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (t, _) -> eval [] t


-- Nyílt kifejezések (röviden)
--------------------------------------------------------------------------------

-- + érték a szabad változó
data Val'
  = VVar' String      -- szabad változó
  | VApp' Val' Val'   -- (szabad változó alkalmazása függvényként)
  | VAdd' Val' Val'   -- (elakadt összeadás, ahol valamelyik operandusban van szabad var)
  | VIntLit' Int
  | VLam' String
        [(String, Maybe Val')]
        Tm
  deriving Show

-- környezet: nem minden változó van feltétlenül definiálva (Maybe Val')
eval' :: [(String, Maybe Val')] -> Tm -> Val'
eval' env t = case t of
  Var x     -> case lookup x env of
                 Nothing -> error ("variable not in scope: " ++ x)
                 Just mv -> case mv of
                   Nothing -> VVar' x   -- nincs definíció
                   Just v  -> v        -- van definíció
  App t u   -> case eval' env t of
                 VLam' x env' t -> eval' ((x, Just (eval' env u)):env') t
                 VIntLit' _     -> error "expected function in application"
                 t              -> VApp' t (eval' env u)
  Lam x t   -> VLam' x env t
  Let x t u -> eval' ((x, Just (eval' env t)):env) u
  IntLit n  -> VIntLit' n
  Add t u   -> case (eval' env t, eval' env u) of
                 (VIntLit' n, VIntLit' n') -> VIntLit' (n + n')
                 (t, u)                    -> VAdd' t u

run' :: [(String, Maybe Val')] -> String -> Val'
run' env str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (t, _) -> eval' env t

-- példa: run' [("x", Nothing)] "x + (100 + 200)" == VAdd' (VVar' "x") (VIntLit' 300)
--   (parciális kiértékelés: amit el lehet végezni számítás, azt elvégezzük)
--   (használható pl: compile time kiértékelés)

-- folytatás:
-- normalizálás: Val' -> Tm
-- visszaadja azt a Tm-et, amiben minden lehetséges compile-time számítás el van végezve
-- pl: (lam x. x + (100 + 200))  --->  (lam x. x + 300)
