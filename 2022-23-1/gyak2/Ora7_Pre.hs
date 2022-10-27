{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Ora7 where

import Control.Applicative
import Control.Monad
import Data.Char

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


data MyTree a = MyLeaf a | MyBranch (MyTree a) a (MyTree a) deriving (Functor, Foldable)
data List a = Nil | Cons a deriving (Functor, Foldable)
data Three a = Three a a a deriving (Functor, Foldable)

-- Definiáljuk az alábboifüggvényeket
-- "Mellékhatásos mappolás"
-- Akkumuláljuk az applikatívokat (szekvenciálás *>-val)

mapThree :: Applicative m => (a -> m b) -> Three a -> m (Three b)
mapThree = undefined

mapList :: Applicative m => (a -> m b) -> List a -> m (List b)
mapList = undefined

mapMyTree :: Applicative m => (a -> m b) -> MyTree a -> m (MyTree b)
mapMyTree = undefined

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
    sequenceA = undefined

instance Traversable Three where
    traverse = mapThree
    sequenceA = undefined

instance Traversable MyTree where
    traverse = mapMyTree
    sequenceA = undefined


-- A traverse-nek egy fő haszna van, a mapM és traverse függvények
-- vegyünk egy traversable-t és írjuk ki az összes elemét stdout-ra
printAll :: (Traversable t, Show a) => t a -> IO ()
printAll = undefined

-- Adjuk hozzá minden kapott értéket az állapothoz
-- mellékhatásba gyűjtsük össze a részeredményt
partialSum :: (Traversable t, Num a) => t a -> State a a
partialSum = undefined

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
    fmap = undefined

instance Applicative Parser where
    pure = undefined
    (<*>) = undefined
    liftA2 = undefined

instance Monad Parser where
    (>>=) = undefined


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
    empty = undefined
    (<|>) = undefined

-- many: 0 vagy többször értékeli ki az alternatívot
-- some: 1 vagy többször, ha nincs 1 se akkor sikertelen

many' :: Alternative f => f a -> f [a]
many' f = some' f <|> pure []

some' :: Alternative f => f a -> f [a]
some' f = liftA2 (:) f (many' f)

-- Elemi parserek

-- Definiáljuk egy parsert ami parse-ol egy fix karaktert
char :: Char -> Parser ()
char = undefined

-- Definiálunk egy parser ami akármilyen karaktert parseol
anyChar :: Parser Char
anyChar = undefined

-- Definiáljuk egy olyan parsert ami egy függvény alapján parseol egy karaktert
satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

-- Definiáljuk újra az első két parsert ennek a segítségével:
char' :: Char -> Parser ()
char' = undefined

anyChar' :: Parser Char
anyChar' = undefined


-- Definiáljuk egy parsert ami eldönti, nincs-e több bemenet
eof :: Parser ()
eof = undefined

-- Definiáljunk egy parsert ami egy ASCII karaktert parseol
-- note: Data.Char.isAscii
ascii :: Parser Char
ascii = undefined

-- Definiáljuk egy parsert ami egy számjegyet beolvas!
-- note: Data.Char.isDigit
-- note: read
digit :: Parser Int
digit = undefined

-- Definiáljuk egy parsert ami egy adott stringet parseol
string :: String -> Parser ()
string = undefined

-- Parserek szekvenciálása és eredményválasztás

-- Definiáljunk egy parsert ami beolvassa a banán szót, majd egy számot
bananaThenDigit :: Parser Int
bananaThenDigit = undefined

-- Definiáljunk egy parsert ami beolvas egy számot, majd a banán szót. Az eredmény a szám legyen
digitThenBanana :: Parser Int
digitThenBanana = undefined

-- Definiáljunk egy parsert ami beolvas egy számjegyet majd majd annyi darab banán szót olvas be
-- note: >>=
-- note: replicateM_
digitBindBananas :: Parser ()
digitBindBananas = undefined

-- Parserek lényege: Monadikus műveletek és az elemei parserek (satisfy, eof) fel tudunk akármilyen parsert építeni a konstruktor használata nélkül

-- Egy kapott listából parseoljuk a szavakat
-- note: mapM_, traverse, void
parseWords :: [String] -> Parser ()
parseWords = undefined


-- Hibakezelés: <|>

-- Definiáljuk az aOrZ parsert ami vagy a-t vagy z-t parseol
aOrZ :: Parser Char
aOrZ = undefined


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
optional' = undefined

charRange :: Char -> Char -> Parser Char
charRange = undefined

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