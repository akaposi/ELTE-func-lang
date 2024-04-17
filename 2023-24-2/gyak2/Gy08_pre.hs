{-# LANGUAGE LambdaCase #-}
module Gy08 where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.List
import Data.Bifunctor

-- Parser hibaüzenettel
type Parser a = StateT String (Except String) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = runExcept (runStateT p s)

(<|>) :: MonadError e m => m a -> m a -> m a
f <|> g = catchError f (const g)
infixl 3 <|>

optional :: MonadError e m => m a -> m (Maybe a)
optional f = Just <$> f <|> pure Nothing

many :: MonadError e m => m a -> m [a]
many p = some p <|> pure []

some :: MonadError e m => m a -> m [a]
some p = (:) <$> p <*> many p

-- Primitívek

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> throwError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> throwError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> throwError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> throwError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> throwError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

-- Eredményes parserek: Olyan parserek amelyeknek van valami eredménye és fel is használjuk őket

-- Parseoljunk be legalább 1 számjegyet!
atLeastOneDigit :: Parser [Int]
atLeastOneDigit = undefined

-- Ennek segítségével tudunk már természetes számokat parseolni
natural :: Parser Int
natural = undefined

-- Parseoljunk be egy egész számot! (Előjel opcionális)
integer :: Parser Int
integer = undefined

-- Bónusz: Float parser (nem kell tudni, csak érdekes)
float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    pure $ s * (r / 10 + fromIntegral i)

-- Definiáljunk egy parsert ami két adott parser között parseol valami mást
-- pl bewteen (char '(') (string "alma") (char ')') illeszkedik az "(alma)"-ra de nem az "alma"-ra
between :: Parser left -> Parser a -> Parser right -> Parser a
between = undefined


-- Definiáljunk egy parsert ami valami elválasztó karakterrel elválasztott parsereket parseol (legalább 1-et)
-- pl
-- runParser (sepBy1 anyChar (char ',')) "a,b,c,d" == Just ([a,b,c,d], "")
-- runParser (sepBy1 anyChar (char ',')) "a" == Just ([a], "")
-- runParser (sepBy1 anyChar (char ',')) "" == Nothing

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 = undefined

-- Ugyanaz mint a fenti, de nem követeli meg, hogy legalább 1 legyen
sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy = undefined

-- Írjunk egy parsert ami egy listaliterált parseol számokkal benne!
-- pl [1,2,30,40,-10]
listOfNumbers :: Parser [Int]
listOfNumbers = undefined

-- Whitespace-k elhagyása
ws :: Parser ()
ws = void $ many $ satisfy isSpace

-- Tokenizálás: whitespace-ek elhagyása
tok :: Parser a -> Parser a
tok p = p <* ws -- Itt a <* kell mert a bal parser eredménye érdekes

topLevel :: Parser a -> Parser a
topLevel p = ws *> tok p <* eof

-- A tokenizált parsereket '-al szoktuk jelölni

natural' :: Parser Int
natural' = tok natural

integer' :: Parser Int
integer' = tok integer

char' :: Char -> Parser ()
char' c = tok $ char c

string' :: String -> Parser ()
string' str = tok $ string str

-- Írjuk újra a listOfNumbers parsert úgy, hogy engedjen space-eket a számok előtt és után illetve a [ ] előtt és után!
goodListofNumbers :: Parser [Int]
goodListofNumbers = undefined

-- Hajtogató parserek

-- Az alábbi parserek kifejezésnyelvekhez lesznek a segédparsereink

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc = undefined

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc  = undefined

-- Nem kötelező HF
-- Olyan parser amit nem lehet láncolni (pl == mert 1 == 2 == 3 == 4 se jobbra se balra nem asszociál tehát nincs értelmezve)
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc = undefined


-- Kompetensebb verziói a fenti parsereknek

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 v op = do
  val <- v
  ( do
      opr <- op
      res <- chainr1 v op
      pure (opr val res)
    )
    <|> pure val

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
  where
    parseLeft val =
      ( do
          opr <- op
          val2 <- v
          parseLeft (opr val val2)
      )
        <|> pure val


-- RDP Algoritmus => Kifejezésnyelv parseolása
-- ADTk segítségével lemodellezzük a programozási nyelvünket:

data Exp
  = IntLit Int -- integer literál pl 1, 2
  | FloatLit Double -- lebegőpontos szám literál, pl 1.0 vagy 2.3
  | Var String -- változónév
  | Exp :+ Exp -- összeadás
  | Exp :* Exp -- szorzás
  | Exp :^ Exp -- hatványozás
  deriving (Eq, Show)

-- Recursive Descent Parsing algoritmus
-- 1, összeírjuk egy táblázatba az operátorok precedeciáját és kötési irányát (ez adott) csökkenő sorrendben
{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési Irány       | Kötési Erősség     |
+--------------------+--------------------+--------------------+
| ^                  | Jobbra             | 16                 |
+--------------------+--------------------+--------------------+
| *                  | Balra              | 14                 |
+--------------------+--------------------+--------------------+
| +                  | Balra              | 12                 |
+--------------------+--------------------+--------------------+
-}
-- 2, Írunk k + 1 parsert, minden operátornak 1 és az atomnak is 1

pAtom :: Parser Exp
pAtom = undefined

pPow :: Parser Exp
pPow = undefined

pMul :: Parser Exp
pMul = undefined

pAdd :: Parser Exp
pAdd = undefined

-- 3,
-- Minden operátor parsernél a kötési irány alapján felépítünk egy parsert
-- Jobbra kötés => rightAssoc vagy chainr1
-- Balra kötés  => leftAssoc vagy chainl1
-- Nem kötő     => nonAssoc
-- Az elválasztó parser az egy tokenizált parsere az operátornak (char' '+', string' "**", stb)
-- Az elválasztandó részkifejezések a táblázatban a fölötte lévő sor parsere (összeadás esetén szorzás, stb), legfelső sor esetén pAtom

-- 4,
-- a pAtom az összes lehetséges literált és változónevet parseol, illetve egy zárójeles kifejezést, ahol a zárójelek között a legalsó sora van a táblázatnak

-- Ha több operátor van ugyanazon a precedencia a szinten akkor <|>-al elválasztjuk őket

-- Extra feladatok
-- Egészítsük ki az alábbi operátorokkal a nyelvet
{-
+--------------------+--------------------+--------------------+
| Operátor neve      | Kötési irány       | Kötési erősség     |
+--------------------+--------------------+--------------------+
| #                  | Balra              | 15                 |
+--------------------+--------------------+--------------------+
| /                  | Jobbra             | 13                 |
+--------------------+--------------------+--------------------+
| -                  | Jobbra             | 13                 |
+--------------------+--------------------+--------------------+
-}

-- Egészítsd ki a nyelvet bool literálokkal  és == nem kötő 10-es erősségű operátorral. Mivel ebben az esetben már a true és false kulcsszó
-- vezessük be a kulcsszavak listáját és a kulcsszó/nem kulcssszó parsert
-- Adjunk a nyelvhez láncolható perfix not operátort és nem-láncolható postfix ! operátort
-- Adjunk a nyelvhez lambda kifejezéseket

keywords :: [String]
keywords = undefined

pNonKeyword :: Parser String
pNonKeyword = undefined

pKeyword :: String -> Parser ()
pKeyword = undefined
