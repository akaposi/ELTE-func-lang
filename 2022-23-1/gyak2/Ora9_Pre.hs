{-# OPTIONS_GHC -Wincomplete-patterns -Wmissing-methods #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, InstanceSigs #-}
module Ora9 where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Char
import Data.Foldable
import Data.List

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) } deriving Functor

instance Applicative Parser where
    (<*>) = ap
    pure a = Parser $ \s -> Just (s,a)

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \s -> pa s >>= uncurry (flip (runParser . f))

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser pa) <|> (Parser pb) = Parser $ \s -> pa s <|> pb s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (xs, x) else Nothing

char :: Char -> Parser ()
char c = void $ satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (\x -> True)

eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ([], ())
    _ -> Nothing

{-
    Regex gyorstalpaló:                                     Haskell megfelelő:
    c          - Parseol egy c karaktert                    char 'c'
    ℓ₁ℓ₂       - Parseol ℓ₁-et majd ℓ₂-t                    ℓ₁ *> ℓ₂ vagy ℓ₁ <* ℓ₂
    ℓ+         - Parseol 1 vagy több ℓ kifejezést           some ℓ
    ℓ*         - Parseol 0 vagy több ℓ kifejezést           many ℓ
    (ℓ₁|ℓ₂)    - Parseol ℓ₁-t vagy ℓ₂-t                     ℓ₁ <|> ℓ₂
    ℓ?         - Parseol 0 vagy 1 ℓ kifejezést              optional ℓ
    .          - Akármilyen karakter                        anyChar
    ℓ{n}       - Parseol n darab ℓ kifejezést               replicateM n ℓ
    $          - Nincs mit parseolni                        eof
    \d         - Parseol egy számjegyet                     satisfy isDigit
    [c₁c₂⋅⋅ cₙ] - Parseolja valamelyiket a karakterek közül  asum $ (\c -> c <$ char c) <$> [c₁,c₂⋅⋅ cₙ] 
    [c₁-c₂]    - c₁ és c₂ között parseol egy karaktert      asum $ (\c -> c <$ char c) <$> [c₁..c₂]
    ℓ{k,}      - legalább k-t parseol ℓ-ből (ℓ{k}ℓ*)        replicateM k ℓ *> many ℓ                 
-}

between :: Parser left -> Parser a -> Parser right -> Parser a
between l a r = l *> a <* r

sepBy :: Parser a -> Parser delim -> Parser [a]
sepBy pa pdelim = sepBy1 pa pdelim <|> pure []

sepBy1 :: Parser a -> Parser delim -> Parser {- nem üres -} [a]
sepBy1 pa pdelim = do
    a <- pa
    delim <- optional $ pdelim
    case delim of
        Just _ -> (\as -> a : as) <$> sepBy1 pa pdelim
        Nothing -> return [a]

digit :: Parser Int
digit = (\a -> ord a - ord '0') <$> satisfy isDigit

natural :: Parser Int
natural = foldl1 (\acc a -> acc * 10 + a) <$> some digit

integer :: Parser Int
integer = do
    l <- optional $ char '-'
    case l of
        Just _ -> negate <$> natural
        _      -> natural

float :: Parser Double
float = do
    s <- (\s -> if null s then 1 else -1) <$> optional (char '-')
    i <- natural
    char '.'
    r <- foldr1 (\a acc -> a + acc / 10) <$> some (fromIntegral <$> digit)
    return $ s * (r / 10  + fromIntegral i)

ws :: Parser ()
ws = void $ many $ satisfy isSpace

tok :: Parser a -> Parser a
tok p = p <* ws

topLevel :: Parser a -> Parser a
topLevel p = ws *> p <* eof

-- Kifejezésnyelv:
data Exp = Lit Integer | Plus Exp Exp | Mul Exp Exp deriving Show

evalExp :: Exp -> Integer
evalExp (Lit n) = n
evalExp (Plus e1 e2) = evalExp e1 + evalExp e2
evalExp (Mul e1 e2) = evalExp e1 * evalExp e2


{-
Kifejezésnyelv parseolása ún. Recursive Precedence Parsing algoritmussal
Ötlet: Precedencia alapján csökkenő sorrendbe áltítjuk a műveleteinket:

    Mul | (*) | Infixl 7
    Add | (+) | Infixl 6

Először +-t próbálunk parseolni, majd *-t majd literált és zárójelet - minden erősségi szintnek 1 db parsere van
Zárójelek után a parseolási sorrend resetelődik

Mi mit parseoljon?

atom: int literál, zárójelezett kifejezés (zárójeles kifejezés meghívja a legerősebb parsert (itt az összeadás))
mul : szorzást (meghívja az atomot)
add : összeadást (meghívja a szorzást)

-}

-- fontos megjezyés: számokat csak a pAtom-ba akarunk parseolni!

-- összeadás kifejezás parseolása
pAdd :: Parser Exp
pAdd = undefined

-- szorzás kifejezés parseolása
pMul :: Parser Exp
pMul = undefined

-- legkisebb szintű kifejezés parseolása (ez esetben literálok és zárójelezett kifejezés)
pAtom :: Parser Exp
pAtom = undefined

-- teljes kifejezés parseolása
pExp :: Parser Exp
pExp = undefined

-- Vezessünk be változókat a nyelvbe!
data Exp' = Lit' Integer | Plus' Exp' Exp' | Mul' Exp' Exp' | Var String deriving Show

evalExp' :: Exp' -> [(String, Exp')] -> Maybe Integer
evalExp' (Lit' n) _ = Just n
evalExp' (Plus' e1 e2) lt = liftA2 (+) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Mul' e1 e2) lt = liftA2 (*) (evalExp' e1 lt) (evalExp' e2 lt)
evalExp' (Var str) lt = case lookup str lt of
    Just exp -> evalExp' exp lt
    _        -> Nothing

-- összeadás kifejezás parseolása
pAdd' :: Parser Exp'
pAdd' = undefined

-- szorzás kifejezés parseolása
pMul' :: Parser Exp'
pMul' = undefined

-- legkisebb szintű kifejezés parseolása (ez esetben literálok, változók és zárójelezett kifejezés)
pAtom' :: Parser Exp'
pAtom' = undefined

-- teljes kifejezés parseolása
pExp' :: Parser Exp'
pExp' = undefined

-- Segédfüggvények:

-- Jobbra asszocialó kifejezést parseoljon.
-- Sep által elválasztott kifejezéseket gyűjtsön össze, majd azokra a megfelelő sorrendbe alkalmazza a függvényt
rightAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
rightAssoc = undefined

-- Ugyanaz mint a rightAssoc csak balra
leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc  = undefined

-- Olyan parser amit nem lehet láncolni (pl == mert 1 == 2 == 3 == 4 se jobbra se balra nem asszociál tehát nincs értelmezve)
nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc = undefined

-- A fentebbi parserek nem tudnak több ugyanolyan kötési erősségü kifejezést parseolni, mert nem differenciálnak az elválasztók között
-- A chain függvények ezt megoldják

chainr1 :: Parser a->  Parser (a -> a -> a) ->  Parser a
chainr1 v op = do
    val <- v
    (do
        opr <- op
        res <- chainr1 v op
        pure (opr val res)
        ) <|> pure val

chainl1 :: Parser a ->  Parser (a -> a -> a) -> Parser a
chainl1 v op = v >>= parseLeft
    where
        parseLeft val = (do
            opr <- op
            val2 <- v
            parseLeft (opr val val2)) <|> pure val

-- Több kifejezés ugyanazon a szinten:

data Exp2 = Add2 Exp2 Exp2 | Sub Exp2 Exp2 | Mul2 Exp2 Exp2 | Lit2 Integer | Var2 String deriving Show

p2Atom :: Parser Exp2
p2Atom = undefined

p2Mul :: Parser Exp2
p2Mul = undefined

p2AddAndSub :: Parser Exp2
p2AddAndSub = undefined

p2Exp :: Parser Exp2
p2Exp = undefined