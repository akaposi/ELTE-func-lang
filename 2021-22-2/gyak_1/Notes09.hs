
{-# language InstanceSigs, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Char  -- isAlpha, isDigit, isAlphaNum, isLower, isUpper, isSpace

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
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

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
  | Var String          -- (változónév)
  deriving (Eq, Show)

-- Parser írása, operátor + változónevek + kulcsszavak

{-
   1. Definiáljuk a ws-t és a primitív parsereket (amik ws-olvasnak)

   2. Definiáljuk a kulcsszó parsert és a változónevek parser-ét

   3. sorojuk fel a nyelvi konstrukciókat kötési erősség szerint
       - atom: zárójelezett kifejezés, literál, változónév   (végtelen kötési erősség)
       - not alkalmazás
       - *  : jobbra asszoc
       - +  : jobbra asszoc
       - -  : jobbra asszoc
       - && : jobbra asszoc
       - || : jobbra asszoc
       - == : nem asszoc       (x == y == z)

   4. Egy parser függvényt írunk minden erősségi szinthez
      Felhasználjuk a library precedencia kombinátorokat.
      Gyengébb függvény meghívja az egyel erősebb függvényt.
      Zárójel belsejében a leggyengébb függvényt hívjuk
      ("recursive precedence parsing" néven lehet rá keresni)

   5. topLevel <leggyengébb parser>    maga a kívánt parser
-}



-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)

data Tm = TmVar String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
