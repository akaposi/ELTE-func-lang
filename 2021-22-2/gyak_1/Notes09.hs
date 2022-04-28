
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Char  -- isAlpha, isDigit, isAlphaNum, isLower, isUpper, isSpace


-- canvas feladat
--------------------------------------------------------------------------------

pBool :: Parser Bool
pBool =  (True  <$ string' "True")
     <|> (False <$ string' "False")

pPairBool :: Parser (Bool, Bool)
pPairBool = do
  char' '('
  b1 <- pBool
  char' ','
  b2 <- pBool
  char' ')'
  pure (b1, b2)

-- p :: Parser (Maybe (Bool, Bool))
-- p = topLevel (
--       (Nothing <$ string' "Nothing")
--   <|> (Just <$> (string' "Just" *> pPairBool)))

pNothing = do
  string' "Nothing"
  pure Nothing

pJust = do
  string' "Just"
  bools <- pPairBool
  pure (Just bools)

pMaybe = pNothing <|> pJust

p :: Parser (Maybe (Bool, Bool))
p = topLevel pMaybe


--------------------------------------------------------------------------------

-- következő canvas:
--   Haskell szintaxisú adat parsolás
--     lista, Maybe, Either, Bool, párok

-- Írj parser-t, ami megfelel a következő regex-nek: <[a-z]+>(,<[a-z]+>)*

-- Parser library
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

-- olvassunk egy karaktert az input elejéről, amire igaz egy feltétel
satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ f = () <$ satisfy f

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
--   optional :: Parser a -> Parser (Maybe a)   -- hibát értékként visszaadja (soha nem hibázik)
--   optional pa = (Just <$> pa) <|> pure Nothing

-- 0 vagy 1 eredményt olvasunk
optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
--   pa psep pa .... psep pa
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

sepBy1_ :: Parser a -> Parser sep -> Parser ()
sepBy1_ pa psep = () <$ sepBy1 pa psep

sepBy_ :: Parser a -> Parser sep -> Parser ()
sepBy_ pa psep = () <$ sepBy pa psep

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

-- token/whitespace parsing segédfüggvények

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
    [e]      -> pure e          -- nem alkalmazunk op-t (default)
    [e1,e2]  -> pure (f e1 e2)  -- pont egy op alkalmazás
    _        -> empty           -- 0 vagy több mint 2 alkalmazás

--------------------------------------------------------------------------------

-- -- csaló megoldás:
-- posInt :: Parser Int
-- posInt = do
--   str <- some (satisfy isDigit)
--   pure $ read str                  -- read :: String -> Int

-- Olvass be egy pozitív Int-et! (Számjegyek nemüres sorozata)
posInt :: Parser Int
posInt = do
  str <- some (satisfy isDigit)  -- str :: String, digitToInt :: Char -> Int

  -- 1.
  let res = fst $ foldr (\c (sum, place) -> (sum + digitToInt c * place, place*10))
                        (0, 1)
                        str

  -- 2.
  -- let digits = map digitToInt str
  -- let digitValues = zipWith (*) (iterate (*10) 1) (reverse digits)
  -- let res = sum digitValues

  -- 3.
  -- let go []      = (0, 1)
  --     go (c:str) = case go str of
  --       (sum, place) -> (sum + digitToInt c * place, place*10)
  -- let res = fst $ go str
  pure res

-- whitespace kezelése:
--   - definiáljuk a whitespace parser-t
--   - minden primitív parser maga után olvasson ws-t
--   - használjuk a topLevel kombinátort a "top"-level parserhez

posInt' :: Parser Int
posInt' = posInt <* ws

-- Írj egy parsert, ami felsimeri Int-ek vesszővel elválasztott listáit!
-- Példák: "[]", "[    12 , 34555 ]", "[0,1,2,3]"
intList :: Parser [Int]
intList = topLevel go where
  -- go :: Parser [Int]
  -- go = do
  --   char' '['
  --   ns <- sepBy posInt' (char' ',')
  --   char' ']'
  --   pure ns

  go :: Parser [Int]
  go = char' '[' *> sepBy posInt' (char' ',') <* char' ']'

maybeInt' :: Parser (Maybe Int)
maybeInt' = nothing <|> just where
  nothing :: Parser (Maybe Int)
  nothing = do
    string' "Nothing"
    pure Nothing

  just :: Parser (Maybe Int)
  just = do
    string' "Just "
    n <- posInt'
    pure (Just n)

-- maybeInt'' :: Parser (Maybe Int)
-- maybeInt'' = (Nothing <$ string' "Nothing")
--          <|> (Just <$> (string' "Just " *> posInt'))

listMaybeInt :: Parser [Maybe Int]
listMaybeInt = topLevel go where

  go :: Parser [Maybe Int]
  go = char' '[' *> sepBy maybeInt' (char' ',') <* char' ']'

-- opcionális házi:
-- pList :: Parser a -> Parser [a]
-- pMaybe :: Parser a -> Parser (Maybe a)
-- listMaybeInt = pList (pMaybe posInt')

-- vagy akár
-- class ParserHaskell a where
--   parse :: Parse a

-- instance ParseHaskell Int
-- instance ParseHaskell a => ParseHaskell [a]
-- instance ParseHaskell a => ParseHaskell (Maybe a)
-- parse :: Parser [Maybe Int]


-- Írj egy parsert, ami [(Bool, Maybe Int)] értékeket olvas Haskell szintaxis szerint!
-- Engedj meg bárhol whitespace-t.
listBoolMaybeInt :: Parser [(Bool, Maybe Int)]
listBoolMaybeInt = topLevel goList where
  goList = char' '[' *> sepBy elem (char' ',') <* char' ']'

  bool = (True  <$ string' "True") <|> (False <$ string' "False")

  elem = (,) <$> (char' '(' *> bool <* char' ',')
             <*> (maybeInt' <* char' ')')

-- (bónusz)
-- Írj egy parsert, ami pontosan a kiegyensúlyozott zárójel-sorozatokat ismeri fel!
-- Helyes példák: "", "()", "()()", "(())()", "(()())", "((()())())"
balancedPar :: Parser ()
balancedPar = many_ (char '(' *> balancedPar *> char ')')




-- Írj egy parser-t a következő kifejezésekhez:
--------------------------------------------------------------------------------

data Exp =
    IntLit Int          -- int literál (pozitív)
  | Add Exp Exp         -- e + e
  | Sub Exp Exp         -- e - e
  | Mul Exp Exp         -- e * e
  | BoolLit Bool        -- true|false
  | And Exp Exp         -- e && e
  | Or Exp Exp          -- e || e
  | Not Exp             -- not e
  | Eq Exp Exp          -- e == e
  | Var String          -- (változónév: nemüres betűsorozat)
  deriving (Eq, Show)

{-
  (definiáljuk ws-t, topLevel-t és a primitív parsereket)

  Kötési erősségek (csökkenő sorrendben)
    - atom : int literál, bool literál, zárojelezett Exp, változók
    - not exp
    - *   (jobb asszociatív)
    - +   (jobb asszociatív)
    - -   (jobb asszociatív)
    - &&  (jobb asszociatív)
    - ||  (jobb asszociatív)
    - ==  (nem asszociatív)

  - Minden erősségi szinthez írunk egy függvényt
  - Minden függvény a következő legerősebb függvényt hívja
    (minden függvény a saját szintjét próbája olvasni,
     viszont default esetként meghívja a következő függvényt)
  - Zárójel belsejében a leggyengébb függvényt hívjuk
  - topLevel parser a leggyengébb parser függvény lesz

  Kulcsszavak vs változók:
    - listázzuk a kulcsszavakat
    - változó nem lehet kulcsszó
    - kulcsszó nem folytatódhat legális változó karakterekkel
      (true, falsexx)

  lásd: "recursive precedence parsing"
-}

keywords :: [String]
keywords = ["not", "true", "false"]

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

pAtom :: Parser Exp
pAtom =  (IntLit <$> posInt')
     <|> (BoolLit True <$ keyword' "true")
     <|> (BoolLit False <$ keyword' "false")
     <|> (Var <$> ident')
     <|> (char' '(' *> pEq <* char' ')')

pNot :: Parser Exp
pNot =  (Not <$> (keyword' "not" *> pAtom))
    <|> pAtom

-- notExp * notExp * ... * notExp
pMul :: Parser Exp
pMul = rightAssoc Mul pNot (char' '*')

-- mulExp + mulExp + ... + mulExp
pAdd :: Parser Exp
pAdd = rightAssoc Add pMul (char' '+')

pSub :: Parser Exp
pSub = rightAssoc Sub pAdd (char' '-')

pAnd :: Parser Exp
pAnd = rightAssoc And pSub (string' "&&")

pOr :: Parser Exp
pOr = rightAssoc Or pAnd (string' "||")

pEq :: Parser Exp
pEq = nonAssoc Eq pOr (string' "==")

pExp :: Parser Exp
pExp = topLevel pEq


-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = TmVar String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
