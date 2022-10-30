{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Ora7 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor

newtype State s a = State { runState :: s -> (s,a) } deriving Functor

instance Applicative (State s) where
    (<*>) = ap
    pure = State . flip (,)

instance Monad (State s) where
    (State a) >>= f = State $ \s -> let (s', a') = a s in runState (f a') s'


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put = State . const . flip (,) ()

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

-- Olvassunk be n számot a terminálról majd szorozzuk őket össze! (1 pont)
productIO :: Int -> IO Int
productIO n = product <$> replicateM n (readLn :: IO Int)

{-
do
    xs <- replicateM n (readLn :: IO Int)
    pure $ product xs
-}
 
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Eq, Show)
 
infixr 5 `Branch`
 
-- Címkézzük meg a fát jobbról balra! bejárásban! (1 pont, egyéb bejárás esetén 0.5 pont)
-- Használjunk state-t!
labelBackwards :: Tree a -> Tree (Int, a)
labelBackwards tr = snd (runState (go tr) 0)
    where
        go :: Tree a -> State Int (Tree (Int, a))
        go (Leaf a) = do
            i <- get
            put (i + 1)
            pure (Leaf (i + 1, a))
        go (Branch l r) = do
            r' <- go r
            l' <- go l
            pure (Branch l' r')
 
{- Tesztesetek:
labelBackwards t1 == (Leaf (3, "alma") `Branch` (Leaf (2, "balma") `Branch` Leaf (1, "calma")))
labelBackwards (Leaf "a" `Branch` Leaf "b") == (Leaf (2, "a") `Branch` Leaf (1, "b"))
-}
 
 
t1 :: Tree String
t1 = Leaf "alma" `Branch` (Leaf "balma" `Branch` Leaf "calma")


data MyTree a = MyLeaf a | MyBranch (MyTree a) a (MyTree a) deriving (Functor, Foldable)
data List a = Nil | Cons a (List a) deriving (Functor, Foldable)
data Three a = Three a a a deriving (Functor, Foldable)

-- Definiáljuk az alábbi függvényeket
-- "Mellékhatásos mappolás"
-- Akkumuláljuk az applikatívokat (szekvenciálás *>-val)

mapThree :: Applicative m => (a -> m b) -> Three a -> m (Three b)
mapThree f (Three a1 a2 a3) = Three <$> f a1 <*> f a2 <*> f a3

-- Three <$> f a1 <*> f a2 <*> f a3
-- liftA3 Three (f a1) (f a2) (f a3)

mapList :: Applicative m => (a -> m b) -> List a -> m (List b)
mapList f Nil = pure Nil
mapList f (Cons a as) = Cons <$> f a <*> mapList f as

mapMyTree :: Applicative m => (a -> m b) -> MyTree a -> m (MyTree b)
mapMyTree f (MyLeaf a) = MyLeaf <$> f a
mapMyTree f (MyBranch l a r) = MyBranch <$> mapMyTree f l <*> f a <*> mapMyTree f r

-- Ez egy létező típusosztály
{-
:i Traversable
type Traversable :: (* -> *) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
-}

-- Most megírtuk a traverse műveleteket, írjuk meg a másikat is!

instance Traversable List where
    traverse = mapList
    sequenceA Nil = pure Nil
    sequenceA (Cons a as) = Cons <$> a <*> sequenceA as

instance Traversable Three where
    traverse = mapThree
    sequenceA (Three a1 a2 a3) = Three <$> a1 <*> a2 <*> a3

instance Traversable MyTree where
    traverse = mapMyTree
    sequenceA (MyLeaf a) = MyLeaf <$> a
    sequenceA (MyBranch l a r) = MyBranch <$> sequenceA l <*> a <*> sequenceA r

data MyData a = Tuple a a deriving (Foldable, Functor, Traversable)



-- A traverse-nek egy fő haszna van, a mapM és traverse függvények
-- vegyünk egy traversable-t és írjuk ki az összes elemét stdout-ra
printAll :: (Traversable t, Show a) => t a -> IO ()
printAll t = void $ traverse print t

-- Adjuk hozzá minden kapott értéket az állapothoz
-- mellékhatásba gyűjtsük össze a részeredményt
partialSum :: (Traversable t, Num a) => t a -> State a (t a)
partialSum t = traverse (\a -> do
        i <- get
        put (i + a)
        pure i
    ) t

-- State-hez hasonló konstrukció: Parser

-- Ez is maga egy függvény ami parseol
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
-- data Parser a = Parser (String -> Maybe (String, a))

{-
A kettő hasonló
    newtype State s a = State  { runState  :: s      ->       (s     , a) }
    newtype Parser  a = Parser { runParser :: String -> Maybe (String, a) }
-}

-- Lényegében egy specializált State, ahol a tuple-t Maybebe csomagoltuk?
-- De miért?
-- A parsernek lehet olyan bemenetet adni, amit nem tud parseolni. Ekkor nincs eredmény (a Maybe Nothing lesz)

instance Functor Parser where
    fmap f (Parser p) = Parser $ \st -> case p st of
        Nothing -> Nothing
        Just (st', a) -> Just (st', f a)

instance Applicative Parser where
    pure a = Parser $ \st -> Just (st, a)
    liftA2 f (Parser p1) (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> case p2 st' of
            Nothing -> Nothing
            Just (st'', b) -> Just (st'', f a b)

instance Monad Parser where
    (Parser p1) >>= f = Parser $ \st -> case p1 st of
        Nothing -> Nothing
        Just (st', a) -> runParser (f a) st'


-- Új típusosztály a hibák kezelésére: Alternative
{-
:i
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
-}

-- Az empty egy garantáltan hibás eset
-- A (<|>) lekezeli a hibás esetet (parser esetén csak akkor futtatja le a jobb oldali parsert ha a bal oldali hibás)

instance Alternative Parser where
    empty = Parser $ \st -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \st -> case p1 st of
        Nothing -> p2 st
        Just (st', a) -> Just (st', a)

-- many: 0 vagy többször értékeli ki az alternatívot
-- some: 1 vagy többször, ha nincs 1 se akkor sikertelen

many' :: Alternative f => f a -> f [a]
many' f = some' f <|> pure []

some' :: Alternative f => f a -> f [a]
some' f = liftA2 (:) f (many' f)

-- Elemi parserek

-- Definiáljuk egy parsert ami parse-ol egy fix karaktert
char :: Char -> Parser ()
char c = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if c == x then Just (xs, ()) else Nothing

-- Definiálunk egy parser ami akármilyen karaktert parseol
anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> Just (xs, x)

-- Definiáljuk egy olyan parsert ami egy függvény alapján parseol egy karaktert
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (xs, x) else Nothing

-- Definiáljuk újra az első két parsert ennek a segítségével:
char' :: Char -> Parser ()
char' c = void $ satisfy (\c' -> c' == c) -- satisfy (== c)

anyChar' :: Parser Char
anyChar' = satisfy (\x -> True)


-- Definiáljuk egy parsert ami eldönti, nincs-e több bemenet
eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Just ([], ())
    _ -> Nothing

-- Definiáljunk egy parsert ami egy ASCII karaktert parseol
-- note: Data.Char.isAscii
ascii :: Parser Char
ascii = satisfy isAscii -- anyChar >>= \c -> if isAscii c then pure c else empty

-- Definiáljuk egy parsert ami egy számjegyet beolvas!
-- note: Data.Char.isDigit
-- note: read
digit :: Parser Int
digit = (\c -> read [c]) <$> satisfy isNumber

-- Definiáljuk egy parsert ami egy adott stringet parseol
string :: [Char] -> Parser ()
string [] = pure ()
string (x:xs) = char x >> string xs

-- Parserek szekvenciálása és eredményválasztás

-- Definiáljunk egy parsert ami beolvassa a banán szót, majd egy számot
bananaThenDigit :: Parser Int
bananaThenDigit = do
    string "banana"
    i <- digit
    return i

-- string "banana" >> digit
-- string "banana" *> digit

-- Definiáljunk egy parsert ami beolvas egy számot, majd a banán szót. Az eredmény a szám legyen
digitThenBanana :: Parser Int
digitThenBanana = do
    i <- digit
    string "banana"
    return i

-- digit <* string "banana"

-- Definiáljunk egy parsert ami beolvas egy számjegyet majd majd annyi darab banán szót olvas be
-- note: >>=
-- note: replicateM_
digitBindBananas :: Parser ()
digitBindBananas = do
    i <- digit
    replicateM_ i (string "banana")

-- digit >>= \i -> replicateM_ i (string "banana")

-- Parserek lényege: Monadikus műveletek és az elemei parserek (satisfy, eof) fel tudunk akármilyen parsert építeni a konstruktor használata nélkül

-- Egy kapott listából parseoljuk a szavakat
-- note: mapM_, traverse, void
parseWords :: [String] -> Parser ()
parseWords strs = void $ traverse (\s -> string s) strs


-- Hibakezelés: <|>

-- Definiáljuk az aOrZ parsert ami vagy a-t vagy z-t parseol
aOrZ :: Parser Char
aOrZ = ('A' <$ char 'A') <|> ('Z' <$ char 'Z')


-- Regex féle parserek
{-
    Regex gyorstalpaló:                               Haskell megfelelő:
    c        - Parseol egy c karaktert                char 'c'
    ℓ+       - Parseol 1 vagy több ℓ kifejezést       some ℓ
    ℓ*       - Parseol 0 vagy több ℓ kifejezést       many ℓ
    (ℓ₁|ℓ₂)  - Parseol ℓ₁-t vagy ℓ₂-t                 ℓ₁ <|> ℓ₂
    ℓ?       - Parseol 0 vagy 1 ℓ kifejezést          optional ℓ
    .        - Akármilyen karakter                    anyChar
    ℓ{n}     - Parseol n darab ℓ kifejezést           replicateM n ℓ
    $        - Nincs mit parseolni                    eof
    \d       - Parseol egy számjegyet                 satisfy isDigit
    [c₁-c₂]  - c₁ és c₂ között parseol egy karaktert  <Még nem írtuk meg>
-}

optional' :: Parser a -> Parser (Maybe a)
optional' p = (Just <$> p) <|> pure Nothing

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = oneOf [c1..c2]
    where
        oneOf [] = empty
        oneOf (x:xs) = (x <$ char x) <|> oneOf xs

-- Írjuk meg az alábbi regex-et parserként

-- (alma|banana)?
p1 :: Parser ()
p1 = undefined

-- [A-Z]{10}
p2 :: Parser ()
p2 = undefined

-- \d{2}
p3 :: Parser ()
p3 = undefined

-- \d+.*$
p4 :: Parser ()
p4 = undefined

-- \d?alma.?banana
p5 :: Parser ()
p5 = undefined