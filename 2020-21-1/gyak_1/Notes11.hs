{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
-- import Data.Foldable -- maximumBy
import Data.Char

--------------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char

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

--------------------------------------------------------------------------------


data Exp
  = BoolLit Bool       -- true, false
  | StringLit String   -- string literál: idézőjelek között 0 vagy több nem-idézőjel karakter
  | Eq Exp Exp         -- e1 == e2
  | Concat Exp Exp     -- e1 ++ e2
  deriving (Show, Eq)

ws :: Parser ()
ws = many_ (satisfy (\c -> c == ' ' || c == '\n'))

char' c     = char c <* ws
string' str = string str <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

pStringLit' :: Parser String
pStringLit' = char '"' *> many (satisfy (/= '"')) <* char '"' <* ws

pAtom :: Parser Exp
pAtom = (BoolLit True  <$ string' "true")
    <|> (BoolLit False <$ string' "false")
    <|> (StringLit <$> pStringLit')

pConcat :: Parser Exp
pConcat = foldr1 Concat <$> sepBy1 pAtom (string' "++")

pEq :: Parser Exp
pEq = do
  e1 <- pConcat
  (Eq e1 <$> (string' "==" *> pConcat)) <|> pure e1

-- pEq' :: Parser Exp
-- pEq' =
--     (Eq <$> pConcat <*> (string' "==" *> pConcat))
--  <|> pConcat

pExp :: Parser Exp
pExp = topLevel pEq


-- https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2019-20-2/vizsga_minta

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

-- Tree
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node1 a (Tree a) | Node2 (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

-- példa érték
ex2 :: Tree Int
ex2 =
  Node1 0
    (Node1 2
       (Node2
          (Node1 10 (Leaf 20))
          (Leaf 30)))

instance Functor Tree where
  fmap f (Leaf a)    = Leaf (f a)
  fmap f (Node1 a t) = Node1 (f a) (fmap f t)
  fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

instance Foldable Tree where
  -- elég fodldr vagy foldMap
  foldr f b (Leaf a)    = f a b
  foldr f b (Node1 a t) = f a (foldr f b t)
  foldr f b (Node2 l r) = foldr f (foldr f b r) l

-- data Ordering = LT | EQ | GT

maximumBy :: Foldable t => (a -> a -> Bool) -> t a -> a
maximumBy lte ta = foldr1 go ta where
  go a a' | lte a a'  = a'
          | otherwise = a

-- Definiáljunk egy függvényt, ami visszadja egy fában a leggyakoribb értéket! Ha
-- több ilyen is létezik, akkor bármelyiket közülük.
mostCommon :: Eq a => Tree a -> a
mostCommon t =
  let as  = foldr (:) [] t
      as' = map (\a -> (a, length $ filter (==a) as)) as
  in fst $ maximumBy (\(a, n) (a', n') -> n <= n') as'

-- Alternatív verzió, listává alakítás nélkül:
mostCommon' :: Eq a => Tree a -> a
mostCommon' t =
  let t' = fmap (\a -> (a, sum $ fmap (\a' -> if a == a' then 1 else 0) t)) t
  in fst $ maximumBy (\(a, n) (a', n') -> n <= n') t'


-- Definiáljuk azt a függvényt, ami visszaadja a legutolsó (leginkább jobboldali)
-- `a` értéket egy fából. Példa: `rightmost ex1 == 30`.
rightmost :: Tree a -> a
rightmost = undefined

-- Definiáljunk egy függvényt, ami visszadja balról az első `a` típusú értéket egy
-- fából, amire igaz egy feltétel. Ha nincs ilyen érték, akkor `Nothing` az
-- eredmény. Példa: `findElem (>10) ex1 == Just 20`
findElem :: (a -> Bool) -> Tree a -> Maybe a
findElem = undefined


instance Traversable Tree where
  traverse f (Leaf a)    = Leaf <$> f a
  traverse f (Node1 a t) = Node1 <$> f a <*> traverse f t
  traverse f (Node2 l r) = Node2 <$> traverse f l <*> traverse f r

-- fmap-ből traverse-t úgy kapunk, hogy fmap-et traverse-re átírjuk, + applicative map-el térünk vissza
-- instance Functor Tree where
--   fmap f (Leaf a)    = Leaf (f a)
--   fmap f (Node1 a t) = Node1 (f a) (fmap f t)
--   fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)


-- Definiáljuk a függvényt, amely megszámozza egy `Tree` elemeit! A bejárás
-- sorrendje legyen balról-jobbra preorder, azaz először a `Node1`-ben található
-- `a` értéket járjuk be, utána pedig a részfát. Tipp: használjuk a `State`
-- monádot.
numberElems' :: Tree a -> Tree (a, Int)
numberElems' = undefined

-- Cseréljük ki egy fában levő `a` értékeket egy adott lista elemeire,
-- balról-jobbra preorder sorrendben. Tipp: használjuk a `State` monádot.
replace :: [a] -> Tree a -> Tree a
replace = undefined


-- Cseréljük ki egy fában levő `a` értékeket egy adott lista elemeire periodikusan,
-- balról-jobbra bejárási sorrendben. A periodikusan azt jelenti, hogyha a lista
-- végére értunk, a következő elem helyére ismét a lista legelső elemét tegyük. Ha
-- a lista eredetileg is üres volt, akkor hagyjuk a fát helyben. Tipp: használjuk a
-- `State` monádot.
periodicReplace :: [a] -> Tree a -> Tree a
periodicReplace = undefined


-- Egz fában lévő `k` típusú értékeket cseréljük le egy kulcs-érték párokat
-- tartalmazó lista értékeire! Azaz keressük ki, hogy a fában lévő kulcshoz milyen
-- érték tartozik a listában. Ha akár egyetlen egy olyan kulcs is előfordul,
-- amelyhez nem tartozik érték, akkor az eredmény lehet `Nothing`! Segítség:
-- használjuk a `traverse` és `lookup` függvényeket! __(3 pont)__

lookupReplace :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupReplace = undefined


-- Rose Tree
--------------------------------------------------------------------------------

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Ord, Show)

-- példa érték
ex1 :: RoseTree Int
ex1 = Branch 2 $
      [ Branch 3 $
          [ Branch 11 []
          ]
      , Branch 5 $ []
      , Branch 7 $
          [ Branch 13 []
          ]
      ]

instance Functor RoseTree where
  fmap = undefined

instance Foldable RoseTree where
  foldr = undefined

-- Definiáljuk azt a függvényt, amely megszámolja hogy hány elem van egy `RoseTree`-ben!
countElems :: RoseTree a -> Int
countElems = undefined

-- Definiáljuk azt a függvényt, amely megkeresi a maximális elemet egy `RoseTree`-ben!
maxElem :: Ord a => RoseTree a -> a
maxElem = undefined

instance Traversable RoseTree where
  traverse = undefined

-- Definiáljuk azt a függvényt, amely megszámozza egy `RoseTree` elemeit! A bejárás
-- sorrendje legyen preorder, azaz először az elemet látogassuk meg, majd balról
-- jobbra a részfákat.
numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems = undefined

-- Definiáljuk azt a függvényt, amely biztonságosan indexel egy listába!
safeIndex :: [a] -> Int -> Maybe a
safeIndex = undefined

-- Definiáljuk azt a függvényt, amely egy `RoseTree`-ben lévő indexet lecserél egy
-- adott lista annyiadik elemére! Az indexelés nem feltétlenül helyes, ezért
-- `Maybe`-vel térjünk vissza! Ha akár egyszer is invalid lenne az index, akkor
-- `Nothing`-gal térjünk vissza!
transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList = undefined
