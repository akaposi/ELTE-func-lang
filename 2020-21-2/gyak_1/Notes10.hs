
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Control.Monad.State
import Debug.Trace
import Data.Foldable

--------------------------------------------------------------------------------

-- "spaced repetition"

-- Következő BEAD feladat: bármilyen "vizsga jellegű kisfeladat"
--    (fák, rekurzív fv, magasabbrendű fv, State, Maybe, Functor, Foldable, Traversable)

-- Vizsgasor:
--   50% : kisfeladat: valamilyen fa típus, Functor, Foldable, Traversable,
--         magasabbrendű függvény, State/Maybe monádikus függvény

--   50% : A mai előadáson látott "While" nyelv parser+interpreter, adjunk hozzá új operátort, nyelvi feature-t
--           példák: https://github.com/AndrasKovacs/ELTE-func-lang/tree/master/2019-20-2/vizsga_minta
--         - Adjunk String típusú értéket + néhány String műveletet a nyelvhez
--         - Adjunk Read/Print műveletet
--         - Adjunk pár értékeket (e1, e2), fst, snd

--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)

  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing      -> Nothing
      Just (a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

-- üres input
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- feltételnek megfelelő karakter
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

-- konkrét karakter
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- bármilyen karakter
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- konkrét string
string :: String -> Parser ()
string str = mapM_ char str

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep >> pa)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

many_ :: Parser a -> Parser ()
many_ p = () <$ many p

some_ :: Parser a -> Parser ()
some_ p = () <$ some p

-- Beolvasunk először egy b-t, utána 0 vagy több a-t, az eredményeket
-- balra asszociálva kombináljuk az adott függvénnyel.
chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f pb pa = do {b <- pb; go b} where
  go b = (do {a <- pa; go (f b a)}) <|> pure b

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

string' :: String -> Parser ()
string' str = string str <* ws

char' :: Char -> Parser ()
char' c = char c <* ws

posInt' :: Parser Int
posInt' = (read <$> some (satisfy isDigit)) <* ws

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

debug :: String -> Parser ()
debug msg = Parser $ \s -> trace (msg ++ "  input: " ++ s) (Just ((), s))

infixLeft :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixLeft pa psep combine = foldl1 combine <$> sepBy1 pa psep

infixRight :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixRight pa psep combine = foldr1 combine <$> sepBy1 pa psep

infixNonAssoc :: Parser a -> Parser sep -> (a -> a -> a) -> Parser a
infixNonAssoc pa psep combine = do
  exps <- sepBy1 pa psep
  case exps of
    [exp]        -> pure exp                  -- 1 db pa kifejezés
    [exp1, exp2] -> pure $ combine exp1 exp2  -- exp1 `psep` exp2
    _            -> empty                     -- exp1 `psep` exp2 `psep` exp3 ... expN


-- BEAD megoldás
--------------------------------------------------------------------------------


data Exp = IntLit Int | Mul Exp Exp | Add Exp Exp | Less Exp Exp | Eq Exp Exp
  deriving (Eq, Show)

atom = (IntLit <$> posInt') <|> (char' '(' *> eq <* char' ')')
mul  = infixLeft atom (char' '*') Mul
add  = infixLeft mul  (char' '+') Add
less = infixNonAssoc add (char' '<') Less
eq   = infixNonAssoc less (string' "==") Eq

parseExp :: Parser Exp
parseExp = topLevel eq

tests = [
    runParser parseExp "10" == Just (IntLit 10,"")
  , runParser parseExp "10 + 20 * 10" == Just (Add (IntLit 10) (Mul (IntLit 20) (IntLit 10)),"")
  , runParser parseExp "0 == 0 + 2" == Just (Eq (IntLit 0) (Add (IntLit 0) (IntLit 2)),"")
  , runParser parseExp "(2 + 4) * 3" == Just (Mul (Add (IntLit 2) (IntLit 4)) (IntLit 3),"")
  , runParser parseExp "1 < 3 == 2" == Just (Eq (Less (IntLit 1) (IntLit 3)) (IntLit 2),"")
  , runParser parseExp "1 < (3 == 2)" == Just (Less (IntLit 1) (Eq (IntLit 3) (IntLit 2)),"")
  ]

--------------------------------------------------------------------------------

-- keyword vs név/azonosító egyértelműsítés:
--    (while parser-ben benne van, EA-ban is szerepel)
-- Házi: megnézni EA anyagban / while implementációban


-- Írj parser-t típusozatlan lambda kalkulushoz!
--------------------------------------------------------------------------------

-- példák : \f x y. f y y
--          (\x. x) (\x. x)
--          (\f x y. f) x (g x)
--          f x y z
--          (f (g x)) (\x. x)

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined


-- Vizsga jellegű kisfeladatok
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

-- írd meg az instance-okat!
instance Functor Tree where
  fmap f (Leaf a)    = Leaf (f a)
  fmap f (Node1 a t) = Node1 (f a) (fmap f t)
  fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

instance Foldable Tree where
  foldr f b (Leaf a)    = f a b                     -- foldr f b [a]    == f a b
  foldr f b (Node1 a t) = f a (foldr f b t)         -- foldr f b (a:as) == f a (foldr f b as)
  foldr f b (Node2 l r) = foldr f (foldr f b r) l   --  f a (f b (f c x))
                                                    -- foldr (||) False

  -- foldMap (házi feladat utánanézni)

-- Definiáljuk azt a függvényt, ami visszaadja a legutolsó (leginkább jobboldali)
-- `a` értéket egy fából. Példa: `rightmost ex1 == 30`.
rightmost :: Tree a -> a
rightmost (Leaf a)    = a
rightmost (Node1 a t) = rightmost t
rightmost (Node2 l r) = rightmost r

-- Definiáljunk egy függvényt, ami visszadja balról az első `a` típusú értéket egy
-- fából, amire igaz egy feltétel. Ha nincs ilyen érték, akkor `Nothing` az
-- eredmény. Példa: `findElem (>10) ex1 == Just 20`
findElem :: (a -> Bool) -> Tree a -> Maybe a
findElem f t = foldr (\a ma -> if f a then Just a else ma) Nothing t
  -- find  -- Data.Foldable függvény

-- írd meg az alábbi instance-ot!
instance Traversable Tree where
  -- fmap f (Leaf a)    = Leaf (f a)
  -- fmap f (Node1 a t) = Node1 (f a) (fmap f t)
  -- fmap f (Node2 l r) = Node2 (fmap f l) (fmap f r)

  traverse f (Leaf a)    = Leaf  <$> f a
  traverse f (Node1 a t) = Node1 <$> f a <*> traverse f t
  traverse f (Node2 l r) = Node2 <$> traverse f l <*> traverse f r


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


-- Definiáljunk egy függvényt, ami visszadja egy fában a leggyakoribb értéket! Ha
-- több ilyen is létezik, akkor bármelyiket közülük. (nehezebb feladat, át lehet ugorni)
mostCommon :: Eq a => Tree a -> a
mostCommon t = undefined


-- Rose Tree
--------------------------------------------------------------------------------

-- (megj: a Rose Tree egy fokkal nehezebb, mint ami várható vizsgában szereplő
--        ADT-ként)

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
