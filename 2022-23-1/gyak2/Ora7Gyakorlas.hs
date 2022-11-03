{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Ora7Gyakrolas where

import Prelude hiding (NonEmpty(..))
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List

-- automatikusan mindig inorder bejárás van implementálva
data One a = One a                                   deriving (Eq, Show, Foldable, Functor)
data NonEmpty a = Last a | Cons a (NonEmpty a)       deriving (Eq, Show, Foldable, Functor)
data Tree a = Leaf a | Node (Tree a) a (Tree a)      deriving (Eq, Show, Foldable, Functor)
data RoseTree a = RoseLeaf a | RoseNode [RoseTree a] deriving (Eq, Show)

instance Foldable RoseTree where
    foldMap f (RoseLeaf a)  = f a
    foldMap f (RoseNode as) = foldMap (foldMap f) as
    foldr f b (RoseLeaf a)  = f a b
    foldr f b (RoseNode as) = foldr (\rt b -> foldr f b rt) b as

instance Functor RoseTree where
    fmap f (RoseLeaf a)  = RoseLeaf $ f a
    fmap f (RoseNode as) = RoseNode $ fmap f <$> as


newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
    pure a = State $ \s -> (s,a)
    (<*>) = ap

instance Monad (State s) where
    State sa >>= f = State $ \s -> let 
        (s', a) = sa s
        (State sb) = f a
            in sb s'

get :: State a a
get = State $ \s -> (s,s) -- State $ join (,)

put :: a -> State a ()
put a = State $ const (a,())

modify :: (a -> a) -> State a ()
modify f = get >>= put . f
{-
do
    s <- get
    put (f s)
-}

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) } deriving Functor

instance Applicative Parser where
    pure a = Parser $ \s -> Just (s, a)
    (<*>) = ap

instance Monad Parser where
    Parser pa >>= f = Parser $ \s -> case pa s of
        Nothing      -> Nothing
        Just (s', a) -> runParser (f a) s'

instance Alternative Parser where
    empty = Parser $ const empty
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

eof :: Parser ()
eof = Parser $ \str -> case str of
    [] -> Just ([], ())
    _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of
    (c:cs) | f c -> Just (cs, c)
    _            -> Nothing

char :: Char -> Parser ()
char c = void $ satisfy (== c)

anyChar :: Parser ()
anyChar = void $ satisfy $ const True

string :: String -> Parser ()
string = mapM_ char

digit :: Parser Int
digit = read <$> singleton <$> satisfy isNumber

range :: Char -> Char -> Parser Char
range x y = asum $ (\c -> c <$ char c) <$> [min x y .. max x y]

{-



FELADATOK KEZDETE




-}
-- Írjunk a fenti típusokra Traversable instance-ot!

instance Traversable One where
    traverse :: Applicative f => (a -> f b) -> One a -> f (One b)
    traverse = undefined
    sequenceA :: Applicative f => One (f a) -> f (One a)
    sequenceA = undefined

instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse = undefined
    sequenceA :: Applicative f => NonEmpty (f a) -> f (NonEmpty a)
    sequenceA = undefined

instance Traversable Tree where
    traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse = undefined
    sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
    sequenceA = undefined

instance Traversable RoseTree where
    traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
    traverse = undefined
    sequenceA :: Applicative f => RoseTree (f a) -> f (RoseTree a)
    sequenceA = undefined

-- Írjuk meg a traverse műveletet sequenceA segítségével! (Csak Functor és Foldable műveletek használhatók illetve egyéb triviális függvények pl id, const stb)
-- A függvényt nyugodtan lehet curryzni és η-redukálni
traverse' :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' f t = sequenceA undefined

-- Írjuk meg a sequenceA műveletet a traverse segítségével! (Csak Functor és Foldable műveletek használhatók illetve egyéb triviális függvények pl id, const stb)
-- A függvényt nyugodtan lehet curryzni és η-redukálni
sequenceA' :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA' t = traverse (\a -> undefined) undefined


-- Feladatok Alternative-al és Traversable-el


-- Definiáljuk a printInOneLine függvényt ami egy sorba kiírja egy Traversable összes elemét!
printInOneLine :: (Traversable t, Show a) => t a -> IO ()
printInOneLine = undefined

-- Csináljuk meg ugyanezt Foldable-el (ez azért oldható meg, mert az eredményt eldobjuk)
printInOneLine' :: (Foldable t, Show a) => t a -> IO ()
printInOneLine' = undefined

-- Definiáljunk egy függvényt, ami egy Traversable-nyi számot kap paraméterül.
-- Minden számhoz olvasson be egy számot stindről, majd adja hozzá!
readAndAdd :: Traversable t => t Int -> IO (t Int)
readAndAdd = undefined

{-
pl.:
readAndAdd [1,2,3,4]
3
5
6
111
[4,7,9,115]
-}

-- Definiáljuk a scanMultiply függvényt, amely minden kapott paramétert összeszoroz az állapottal
-- A részeredmények legyenek a mellékhatások
scanMultiply :: (Num a, Traversable t) => t a -> State a (t a)
scanMultiply = undefined

-- runState (scanMultiply [1,2,3,4]) 5 == (120, [5,5,10,30])

-- Definiáljuk az asum függvényt amely egy hajtogatható tároló elemin végighajtogatja a <|> függvényt!
-- Ez lényegében a oneOf függvény amit órán írtunk, csak ez általánosabb
asum' :: (Foldable f, Alternative t) => f (t a) -> t a -- létező fv Control.Applicative.asum
asum' = undefined

-- Definiáljuk a |> operátort ami hibakezelést végez egy Alternative-en
-- Ha az első paraméter sikertelen adjuk vissza a második paraméterként kapott tiszta értéket
-- Tipp: Minden Alternative egy Applicative is
(|>) :: Alternative f => f a -> a -> f a
f |> a = undefined

infixl 3 |>

-- State-es feladatok

-- Definiáljuk a pop függvényt amely kiszedi a lista első elemét ha tudja
pop :: State [a] (Maybe a)
pop = undefined


-- Definiáljuk a push függvényt ami egy elemet hozzáfűz a listához
push :: a -> State [a] ()
push = undefined

-- Definiáljuk a nubWithState függvényt ami state segítségével leszűri a duplikált elemeket egy listában!
nubWithState :: Eq a => [a] -> [a]
nubWithState = undefined

-- Definiáljuk az intersectWithState függvényt ami state segítségével két lista metszetét számolja ki
intersectWithState :: Eq a => [a] -> [a] -> [a]
intersectWithState = undefined

-- Definiáljuk a unionWithState függvényt  ami state segítségével két lista unióját számolja ki
unionWithState :: Eq a => [a] -> [a] -> [a]
unionWithState = undefined

-- Definiáljuk a labelRoseTree függvényt amely megcímkéz egy rózsafát!
labelRoseTree :: RoseTree a -> RoseTree (a, Int)
labelRoseTree = undefined

-- Definiáljuk a labelTraversable függvényt amely megcímkéz egy tetszőleges Traversable típust!
labelTraversable :: Traversable t => t a -> t (a, Int)
labelTraversable = undefined


-- Parserek

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

-- Definiáljuk a oneOf parsert ami az adott karakter egyikét parseolja
oneOf :: [Char] -> Parser Char
oneOf = undefined

-- Definiáljuk a digits parsert ami parseol legalább 1 db számjegyet
-- Regex: \d+
digits :: Parser [Int]
digits = undefined

-- Definiáljuk az alábbi regex kifejezéseket haskellben

-- (ab)?ba+.*
p0 :: Parser ()
p0 = undefined

-- \d{2,}-?$
p1 :: Parser ()
p1 = undefined

-- [A-Z]*[1,2,3,9]?(A|Z)$
p2 :: Parser ()
p2 = undefined

-- a+b*c?d{4}e{5,}
p3 :: Parser ()
p3 = undefined

-- alm(a|ák)|banán(ok)?
p4 :: Parser ()
p4 = undefined

-- Segédfv
-- Definiáljuk azt a függvényt ami egy listányi számjegyből egy egész számot épít fel
digitsToNum :: [Int] -> Int
digitsToNum = undefined

-- digitsToNum [] == 0
-- digitsToNum [1,2,3] == 123
-- digitsToNum [0,0,2,5,0,3] == 2503

-- definiáljuk a natural parsert ami egy pozitív egész számot parseol!
-- emlékeztető: digits
natural :: Parser Int
natural = undefined

-- definiáljuk a signedInt parsert ami egy előjeles egész számot parseol!
signedInt :: Parser Int
signedInt = undefined

-- Definiáljuk a between parsert ami parseol egy kifejezést két másik parser között!
between :: Parser left -> Parser a -> Parser right -> Parser a
between = undefined