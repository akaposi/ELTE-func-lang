{-# LANGUAGE LambdaCase #-}
module Gy08 where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative
import Data.Char
import Data.List
import Data.Bifunctor
import Control.Monad
import Data.Maybe (fromMaybe)

-- Parser hibaüzenettel
type Parser a = StateT String (Except [String]) a

runParser :: Parser a -> String -> Either String (a, String)
runParser p s = first (\x -> concat $ drop (length x - 1) x) $ runExcept (runStateT p s)

parseError :: String -> Parser a
parseError = throwError . singleton

-- Primitívek


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= \case
  (c:cs) | p c -> c <$ put cs
  _            -> parseError "satisfy: condition not met or string empty"

eof :: Parser ()
eof = get >>= (<|> parseError "eof: String not empty") . guard . null

char :: Char -> Parser ()
char c = void $ satisfy (== c) <|> parseError ("char: not equal to " ++ [c])

anyChar :: Parser Char
anyChar = satisfy (const True)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit <|> parseError "digit: Not a digit"

string :: String -> Parser ()
string str = mapM_ (\c -> char c <|> parseError ("string: mismatch on char " ++ [c] ++ " in " ++ str)) str

-- Eredményes parserek: Olyan parserek amelyeknek van valami eredménye és fel is használjuk őket

-- Parseoljunk be legalább 1 számjegyet!
atLeastOneDigit :: Parser [Int]
atLeastOneDigit = some digit

--- 483 = 480 + 3 = 48 * 10 + 3 = (40 + 8) * 10 + 3 = (( 4 + 0) * 10 + 8) * 10 + 3
-- Ennek segítségével tudunk már természetes számokat parseolni
natural :: Parser Int
natural = foldl (\r e -> r * 10 + e) 0 <$> atLeastOneDigit

-- >>> runParser natural "483"
-- Right (483,"")

-- Parseoljunk be egy egész számot! (Előjel opcionális)
integer :: Parser Int
integer = optional (char '-') >>= \case Nothing -> natural; Just _ -> ((-1) *) <$> natural

-- >>> runParser integer "-483"
-- Right (-483,"")

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
between left o right = left *> o <* right


-- Definiáljunk egy parsert ami valami elválasztó karakterrel elválasztott parsereket parseol (legalább 1-et)
-- pl
-- runParser (sepBy1 anyChar (char ',')) "a,b,c,d" == Just ([a,b,c,d], "")
-- runParser (sepBy1 anyChar (char ',')) "a" == Just ([a], "")
-- runParser (sepBy1 anyChar (char ',')) "" == Nothing

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
--- sepBy1 o delim = (\n -> fromMaybe singleton ( (\ls -> (flip (foldr ($)) ls) . singleton) <$> n) <$> o) =<< optional (some ((:) <$> o <* delim))
sepBy1 o delim = do
  ls <- optional . some $ (:) <$> o <* delim
  end <- o
  pure . fromMaybe [] $ foldr ($) [end] <$>  ls
--sepBy1 o delim = o >>= \a -> (a :) <$> some (delim *> o)

-- >>> runParser (sepBy1 anyChar (char ',')) [',' | _ <- [1..10]]
-- Left "satisfy: condition not met or string empty"

-- Ugyanaz mint a fenti, de nem követeli meg, hogy legalább 1 legyen
sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy a d = optional (sepBy1 a d) >>= pure . fromMaybe [] 

-- Írjunk egy parsert ami egy listaliterált parseol számokkal benne!
-- pl [1,2,30,40,-10]
listOfNumbers :: Parser [Int]
listOfNumbers = between (char '[') (sepBy integer (char ',')) (char ']')

--- >>> runParser listOfNumbers "[]"
-- Right ([],"")

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
goodListofNumbers = between (char' '[') (sepBy integer' (char' ',')) (char' ']')

--- >>> runParser goodListofNumbers "[         42      ,    -3    ]"
-- Right ([42,-3],"")

-- Hajtogató parserek

-- Az alábbi parserek kifejezésnyelvekhez lesznek a segédparsereink

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc f e sep = sepBy1 e sep >>= \case [] -> parseError "rightAssoc : No element in the list!"; (a : ls) -> pure $ foldr f a ls

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f e sep = sepBy1 e sep >>= \case [] -> parseError "leftAssoc : No element in the list!"; (a : ls) -> pure $ foldl f a ls

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

from a b = satisfy (\x -> x >= min a b && x <= max a b)

notDigChar = from 'a' 'z' <|> from 'A' 'Z'

pAtom :: Parser Exp
pAtom = (IntLit <$> integer') <|> (FloatLit <$> float) <|> (Var <$> ( notDigChar >>= \c -> (c :) <$> many (notDigChar <|> from '0' '9') )) <|> between (char '(') pAdd (char ')')

-- :^
pPow :: Parser Exp
pPow = (pAtom >>= \ p1 -> string' "**" *> pAtom >>= \p2 -> pure $ p1 :^ p2) <|> pAtom

pMul :: Parser Exp
pMul = let pars = pPow in (pars >>= \p1 -> char '*' *> pars >>= \p2 -> pure $ p1 :* p2) <|> pPow

pAdd :: Parser Exp
pAdd = (pMul >>= \p1 -> char '+' *> pMul >>= \p2 -> pure $ p1 :+ p2) <|> pMul

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
