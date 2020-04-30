
-- parser monád folytatás

import Prelude hiding (exp)
import Control.Monad
import Control.Applicative    -- many, some
import Data.Char -- isSpace, isLetter, isDigit, isAlpha

-- State String + Maybe monád
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  -- deriving Functor

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s ->
       fmap (\(a, s') -> (f a, s')) (g s)

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

  -- rögtön hibázó parser
  empty = Parser $ \_ -> Nothing

  -- választás két parser között
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- Kiolvasunk egy karaktert az inputból (amire igaz egy feltétel)
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)    -- őrfeltétel: f c
  _          -> Nothing

-- üres inputra illeszkedik
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- konkrét karakter olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)   -- nem érdekel a végeredmény, mivel elve ismerjük a Char-t

-- konkrét String olvasása
string :: String -> Parser ()     -- String = [Char]
string = mapM_ char               --

-- many és some
-- many regex: (_)*
-- some refex: (_)+

------------------------------------------------------------

-- 1. példa: számok listái

digit :: Parser Char
digit = satisfy isDigit

-- kis csalás: Prelude-ben standard osztály: Read a, metódusa: read :: a -> String
numeral :: Parser Int
numeral = read <$> some digit  -- egy vagy több szám karakter

-- Nulla vagy több "a"-t akarok olvasni, "b"-vel elválasztva.
-- Listában adjuk vissza az összes olvasott "a"-t.
-- (parser library: parsec, megaparsec, attoparsec)

-- (segédfüggvény: legalább 1 darab a-t olvasunk)
sepBy1 :: Parser a -> Parser b -> Parser [a]
-- sepBy1 pa pb = do
--   a  <- pa
--   as <- many $ do
--     a <- pa
--     pb
--     pure a
--   pure (a:as)

-- Applicative stílus:
-- Control.Applicative-ból: <*, *>
--   két művelet elvégzése, viszont választunk, hogy az első vagy a második
--   művelet eredményét adjuk vissza
--   pl: pa <* pb    visszaadja a pa eredményét
--       pa *> pb    visszaadja a pb eredményét
--   (csak a visszatérési értéket befolyásolja! Mindkét művelet hatása létrejön)
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

-- akármilyen karakter olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

letter :: Parser Char
letter = satisfy isLetter   -- Data.Char.isLetter

-- runParser (sepBy1 (char 'a') (char 'b')) "abababababa"
-- runParser (sepBy1 (satisfy (\_ -> True)) (char 'b')) "xbybxbybxby"

-- lista: [ 0-vagy több vesszővel elválasztott szám ]

-- monadikus verzió:
numList :: Parser [Int]
numList = do
  char '['
  nums <- sepBy numeral (char ',')
  char ']'
  pure nums

-- applikatív verzió:
numList' :: Parser [Int]
numList' = char '[' *> sepBy numeral (char ',') <* char ']'
  -- gyakori idióma: csőrök arra a kifejezésre néznek, aminek a visszatérési
  -- értékére kíváncsiak vagyunk
  -- pl működik a következő is:
  --    char '[' *> sepBy numeral (char ',') <* char ']' <* char 'x' <* char 'x'
  -- de nem működik:
  --    char '[' *> sepBy numeral (char ',') <* char ']' <* char 'x' *> char 'x'

-- probléma: nem engedjük meg a space-eket sehol
-- be kell vezetni a whitespace olvasást
--   tradicionálisan: lexikális elemzés (lexer), lexer feladata a space-ek eldobása, +
--      a tokenek és literálok felismerése
--   parser kombinátorok: nincs külön lexer, hanem a lexelés a parser program része


-- 1. Lexikális elemzés szervezése (token parsolás)

--    1. definiáljuk a whitespace parsert
--       (ez csak megesz minden whitespace-t)
--       (beletartozhat még: kommentek olvasása)
--    2. definiáljuk a primitív token parsereket (pl, "string" parser, "char" parser)
--       (valahány karaktert egyszerre olvasnak, egybefüggő lexikális elemeket)
--       VISZONT: minden olvasás után megesszük a whitespace-t

-- token parsing numList'-re
-- feladat: hozzuk létre a numList''-t, ami megengedi a szóközöket akárhol

-- 1. whitespace

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

-- 2. token parserek (char, numeral)

char' :: Char -> Parser ()
char' c = char c <* ws

numeral' :: Parser Int
numeral' = numeral <* ws

-- ha mindenhol token parsereket használunk, akkor mindenhol lehet space,
-- kivéve a program legelején, ezért a program elejére még egy ws kell
numList'' :: Parser [Int]
numList'' = ws *> char' '[' *> sepBy numeral' (char' ',') <* char' ']'

-- pl: runParser numList'' " [ 10, 20 , 30 ,  40 ] "

--------------------------------------------------------------------------------

-- 2. Ajánlás: mindig találjuk ki, hogy mi legyen atomi kifejezés

--   "atomi" kifejezés: olyan kifejezés, hogy mindegy, hogy mi van a környezetben,
--       mert lényegében "zárt" a kifejezés.
--   példa: szám literál, zárójeles kifejezés
--        ellenpélda: 10 + 20 nem atomi, mivel ha *-t teszünk elé vagy utána, akkor
--        máshogy asszociál a kifejezés.
--   atomi:     if e1 then e2 else e3 endif      (mivel két oldalról zárva van)
--   nem atomi: if e1 then e2 else e3            (mivel jobb oldalról nyitva van)
--   általában: olyan kifejezés atomi, ami két oldalról le van zárva tokennel
--              vagy pedig maga a kifejezés egyetlen token

-- parsoljuk a következő nyelvet: (), +, *, szám literálok, whitespace bárhol
-- pl: 10 + 20 * 40 * (30 + ((50)))

-- atom olvasása
atom :: Parser Int
atom = numeral' <|> (char' '(' *> sumExp <* char' ')')

-- kötési erősségbeli különbség: + gyengébb, mint a *
-- megoldás:
--   két parser:
--     1. olvassa az összeadásos kifejezéseket: egy vagy több szorzásos kif összege
--     2. olvassa az szorzásos   kifejezéseket: egy vagy több atomi kifejezés szorzata
--     (atom-ba pedig a zárójellel beágyazható a legyengébb kifejezés (összeg))
--
-- Általánosan: a gyengébb művelet parsere meghívja az erősebb művelet parserét
--              a végén leérünk atom-ig, akkor megint a leggyengébbet hívjuk a
--              zárójel parsolásnál

-- 1^2^3 = 1^(2^3)
expExp :: Parser Int
expExp = foldr1 (^) <$> sepBy1 atom (char' '^')

mulExp :: Parser Int
mulExp = product <$> sepBy1 expExp (char' '*')

sumExp :: Parser Int
sumExp = sum <$> sepBy1 mulExp (char' '+')

-- top-level parser: két dolga van
--   - kezdő whitespace-t olvas
--   - illeszkedik az input végére
exp :: Parser Int
exp = ws *> sumExp <* eof

evalExp :: String -> Maybe Int
evalExp str = fst <$> runParser exp str

-- evalExp "1 + (3 + 4) * 10" == Just 71
-- evalExp "1 + 3 + 4 * 10" == Just 44
-- evalExp "1 + 2 * 3"
-- evalExp "10^2^2 * 2 + 10" == Just 20010

-- megcserélni az erősséget:

-- atom :: Parser Int
-- atom = numeral' <|> (char' '(' *> mulExp <* char' ')')

-- sumExp :: Parser Int
-- sumExp = sum <$> sepBy1 atom (char' '+')

-- mulExp :: Parser Int
-- mulExp = product <$> sepBy1 sumExp (char' '*')


-- Összefoglalás
--------------------------------------------------------------------------------

-- 1. Token parsolás! Mindig kezdjük a whitespace-el és a token parserekkel (char, string)
-- 2. Találjuk ki az atomi parsert (legerőseb kifejezés, nem függ a környezettől)
-- 3. Infix/prefix/postfix műveletek erőssége
--      Mindig a gyengébb művelet parsere hívja az erősebbet

-- Library-ben: parsec, operátorok precedencia parsolása
--    (csak fellistázzuk a műveleteket és erősségeket, és megkapjuk a parser-t)
--    érdemes érteni, hogy kézzel hogyan kell precedenciát olvasni

-- köv: konkrét parser + monadikus interpreter
