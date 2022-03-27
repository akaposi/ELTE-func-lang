{-# language DeriveFunctor, InstanceSigs #-}

--------------------------------------------------------------------------------
-- parser folyt, kombinátorok (sepBy etc), debug, adat olvasás
-- token parserek
-- operátor parser
--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Debug.Trace
import Data.Char   -- isSpace, isDigit

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

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional pa

--------------------------------------------------------------------------------

-- trace :: String -> a -> a
-- trace msg a

debug :: String -> Parser a -> Parser a
debug msg pa = Parser $ \s -> trace (msg ++ " : " ++ s) (runParser pa s)

pBool :: Parser Bool
pBool =
  debug "pBool"
  ((True <$ string "True") <|> (False <$ string "False"))
      -- (do {string "True"; pure True}) <|> (do {string "False"; pure False})

pPair :: Parser a -> Parser b -> Parser (a, b)
pPair pa pb = debug "pPair" $ do
  char '('
  a <- pa
  char ','
  b <- pb
  char ')'
  pure (a, b)

p1 :: Parser (Bool, (Bool, Bool))
p1 = pPair pBool (pPair pBool pBool)

-- many  :: Parser a -> Parser [a]       -- 0-szor vagy többször futtatja
-- some  :: Parser a -> Parser [a]       -- 1-szer vagy többször futtatja

-- (<$>), (<$), (<*>), (<*), (*>)

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
  a  <- pa
  as <- many (psep *> pa)
  pure (a:as)

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

pList :: Parser a -> Parser [a]
pList pa = debug "pList" $ do
  char '['
  as <- sepBy pa (char ',')
  char ']'
  pure as

-- összerakhatunk sok parsert a meglévő függvényekből
pListListBool = pList (pList pBool)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- egyszerűsítés: minden konstruktor zárójelben van

pTree :: Parser a -> Parser (Tree a)
pTree pa =
  debug "pTree" (
      (Leaf <$> (string "(Leaf " *> pa <* char ')'))    -- pa értékét adjuk vissza
  <|> (Node <$> (string "(Node " *> pTree pa <* char ' ')
            <*> (pTree pa <* char ')')))

pLeaf :: Parser a -> Parser (Tree a)
pLeaf pa = do
  string "(Leaf "
  a <- pa
  char ')'
  pure (Leaf a)

pNode :: Parser a -> Parser (Tree a)
pNode pa = do
  string "(Node "
  l <- pTreeM pa
  char ' '
  r <- pTreeM pa
  char ')'
  pure (Node l r)

pTreeM :: Parser a -> Parser (Tree a)
pTreeM pa = pLeaf pa <|> pNode pa


-- whitespace kezelés (token parserek)
--------------------------------------------------------------------------------

-- konvenció (token parser konvenció)

-- 1. definiáljuk a whitespace parser-t

ws :: Parser ()
ws = many_ (satisfy isSpace)

-- 2. minden primitív parser maga után olvasson ws-t. Ilyen parser-t hívjuk "token" parsernek
   -- '-s parserek a token parserek

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

-- 3. írjuk meg a parser-t, viszont mindenhol token parser primitíveket használva

-- 4. A tényleges parser-nél kezeljük a kezdő ws-t (és illeszkedünk az input végére)
--    Azaz: használjuk a topLevel kombinátort

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof


-- példa:
pBool' :: Parser Bool
pBool' =
  debug "pBool'"
  ((True <$ string' "True") <|> (False <$ string' "False"))
      -- (do {string "True"; pure True}) <|> (do {string "False"; pure False})

pPair' :: Parser a -> Parser b -> Parser (a, b)
pPair' pa pb = debug "pPair'" $ do
  char' '('
  a <- pa
  char' ','
  b <- pb
  char' ')'
  pure (a, b)

pPairBoolBool = topLevel (pPair' pBool' pBool')


-- Operátorok + precedencia
--------------------------------------------------------------------------------

-- olvassunk olyan kifejezést, amiben lehet számliterál, +, zárójelezés
--     + jobbra asszociál
--     (kezeljük whitespace-t is mindenhol)

-- szintaxisfa:
data Exp = IntLit Int | Add Exp Exp
  deriving (Eq, Show)

-- 10             --> IntLit 10
-- 10 + 20        --> Add (IntLit 10) (IntLit 20)
-- 10 + 20 + 30   --> Add (IntLit 10) (Add (IntLit 20) (IntLit 30))
-- (10 + 20) + 30 --> Add (Add (IntLit 10) (IntLit 20)) (IntLit 30)

-- recursive precedence parsing



-- általános megoldás:
--   - vegyük sorra az operátorokat csökkenő erősség sorrendjében
--   - minden erősségi szinthet írjunk egy függvényt
--   - erősebbik függvény hívja a következő gyengébbet

-- jelenlegi eset:
--   - atom: literál, zárójelezett kifejezés         (olvasás nem függ a jobb és bal környezettől)
--   - összeadás: jobbra asszociál

-- pozitív Int-ek olvasása
pPosInt' :: Parser Int
pPosInt' = do
  digits <- some (satisfy isDigit)
  ws
  pure (read digits)

pAtom :: Parser Exp
pAtom =
  debug "pAtom" (
      (IntLit <$> pPosInt')
  <|> (char' '(' *> pAdd <* char' ')'))        -- zárójel belsejében a leggyengébb parser-t hívjuk

pAdd :: Parser Exp
pAdd = debug "pAdd" (foldr1 Add <$> sepBy1 pAtom (char' '+'))

pExp :: Parser Exp
pExp = topLevel pAdd

-- (rekurzív precedencia parser: call stack-en tartja nyilván a kötési erősségeket)
-- el lehet szórakozni debug nyomtatással

-- "   sdfoskdfoskd  "
--
