
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State


newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c       -> Just (c, cs)
       | otherwise -> Nothing
  [] -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

string :: String -> Parser ()
string str = mapM_ char str

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ p = () <$ many p
some_ p = () <$ some p

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = satisfy (\c -> c1 <= c && c <= c2)

letter :: Parser ()
letter = () <$ (charRange 'a' 'z' <|> charRange 'A' 'Z')

digit :: Parser Int
digit = digitToInt <$> charRange '0' '9'

posInt :: Parser Int
posInt = do
  ds <- some digit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

-- token parser-ek
--------------------------------------------------------------------------------

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' str = string str <* ws

posInt' :: Parser Int
posInt' = posInt <* ws


-- Írj egy parser-t ami, zárójeleket, +-t, *-t és pozitív Int literálokat tartalmazó kifejezéseket olvas!
--------------------------------------------------------------------------------
--   példák: 10 + 20 + 30
--           (10 + 20) + 30 * 1
--           10 + 10 * 20
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen (Plus (Lit 1) (Plus (Lit 2) (Lit 3)))
-- A * operátor kössön erősebben a +-nál, és asszociáljon jobbra.

data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp deriving Show

-- Megoldás menete:
--   1. ws + token parserek    (lexikális elemzés)
--   2. Precedenciák felírása

{- Precedenciák:

- mindig a zárt/atomi kifejezések a legerőssebek
- két oldaról token határolja őket
- nem befolyásolja, hogy mi van előtte vagy utána
- pl: - literál önmagában 1 token
      - ( ... )
      - if ... then ... else ... endif
      - [ ... ]

- ellenpélda: 1 + 2       ---> Plus (Lit 1) (Lit 2)
              1 + 2 * 3   ---> Plus (Lit 1) (Mul (Lit 2) (Lit 3))

1 Atomi kifejezések (literál, zárójelezett kif)
2 *  (jobb asszoc)
3 +  (jobb asszoc)
-}

-- Minden precedencia egy külön függvény
-- Gyengébb függvény hívja a következő erősebb függvényt
-- Atomi parser hívja a leggyengébb függvényt
-- Top-level parser hívja a leggyengébbet

pAtom :: Parser Exp
pAtom =
      (Lit <$> posInt')
  <|> (char' '(' *> pPlus <* char' ')')

-- általános szabály: asszociatív infix operátor:
--   jobb asszoc: foldr1 <konstruktor> <$> sepBy1 <következő legerősebb parser> <operátor>
--   bal  asszoc: foldl1 <konstruktor> <$> sepBy1 <következő legerősebb parser> <operátor>

-- (csak akkor működik, ha minden precedencia-szinten csak egy fajta operátor van)
pMul :: Parser Exp
pMul = foldr1 Mul <$> sepBy1 pAtom (char' '*')

pPlus :: Parser Exp
pPlus = foldr1 Plus <$> sepBy1 pMul (char' '+')

pTopExp :: Parser Exp
pTopExp = ws *> pPlus <* eof

-- rekurzív hívások "1 * 2 + 3" -ra:

-- pPlus "1 * 2 + 3"
--   pMul "1 * 2 + 3"
--     pAtom "1 * 2 + 3"
--     *
--     pAtom "2 + 3"
--   +
--   pMul "3"
--     pAtom "3"
-- OK

-- minél mélyebben van egy függvény hívva, annál erősebben köt!


-- Egészítsük ki a nyelvet Bool literálokkal és egyenlőség-vizsgálattal!
--------------------------------------------------------------------------------

data Exp2 = IntLit2 Int | BoolLit2 Bool | Plus2 Exp2 Exp2 | Mul2 Exp2 Exp2 | Eq2 Exp2 Exp2
            deriving Show

{- Precedencia:

1 Atom: int literál, zárójelezés, true, false
2 *,/   (jobb asszoc)
3 +     (jobb asszoc)
4 ==    (nem  asszoc)

-}

pAtom2 :: Parser Exp2
pAtom2 =
      (IntLit2 <$> posInt')
  <|> (BoolLit2 <$> ((True <$ string' "true") <|> (False <$ string' "false")))
  <|> (char' '(' *> pEq2 <* char' ')')

pMul2 :: Parser Exp2
pMul2 = foldr1 Mul2 <$> sepBy1 pAtom2 (char' '*')

pPlus2 :: Parser Exp2
pPlus2 = foldr1 Plus2 <$> sepBy1 pMul2 (char' '+')

-- nem asszociatív operátor: vagy nincs "==", vagy pedig pontosan egy "==" van használva
pEq2 :: Parser Exp2
pEq2 = do
  e1 <- pPlus2
  (Eq2 e1 <$> (string' "==" *> pPlus2)) <|> pure e1
  --    van ==                              nincs ==

pTopExp2 :: Parser Exp2
pTopExp2 = ws *> pEq2 <* eof


-- írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- var ::= <nemüres lowercase string>
-- exp ::= var | \var. exp | exp exp | ( exp ) | let var = exp in exp

-- példák : \f. \x. \y. f y y
--          (\x. x) (\x. x)
--          (\f. \x. \y. f) x (g x)

{- Precedencia:

1 változó, zárójelezés
2 függvényalkalmazás     infix  (bal asszoc)
3 let, \x.               prefix (jobb asszoc)             (prefix: vagy jobb asszoc, vagy nem asszoc)
                                                          (postfix: vagy bal asszoc, vagy nem asszoc)

- függvényalkalmazás: operátor, ahol a szeparátor nem olvas semmit!
-}

data Tm = Var String | App Tm Tm | Lam String Tm | Let String Tm Tm deriving Show

-- a kulcsszavak és azonosítók nem fedhetik át egymást!
keywords :: [String]
keywords = ["let", "in"]

pIdent' :: Parser String
pIdent' = do
  x <- some (satisfy isLower) <* ws
  if elem x keywords
    then empty
    else pure x

pAtomTm :: Parser Tm
pAtomTm = (Var <$> pIdent') <|> (char' '(' *> pLetLam <* char' ')')

pApp :: Parser Tm
pApp = foldl1 App <$> sepBy1 pAtomTm (pure ())

-- prefix operátor, ami asszociatív: saját magát hívja
-- (default eset: a következő erősebb parser-t próbáljuk)
pLetLam :: Parser Tm
pLetLam =
      (Lam <$> (char' '\\' *> pIdent' <* char' '.') <*> pLetLam)
  <|> (Let <$> (string' "let" *> pIdent' <* char' '=') <*> pLetLam <*> (string' "in" *> pLetLam))
  <|> pApp

-- full backtrack veszélyei:
--     1. exponenciális futás
--     2. hiba kontextusát elveszítjük (akár az input elejére is backtrack-elhetünk!)

-- gyakorlatban library-ben : két féle hiba van:
--    1. failure, amiből backtrack-elni lehet, a parser logikát implementáljuk vele
--    2. error, mindenképp szintaktikai hiba, nem lehet visszakozni belőle

----------

pTopTm :: Parser Tm
pTopTm = ws *> pLetLam <* eof
