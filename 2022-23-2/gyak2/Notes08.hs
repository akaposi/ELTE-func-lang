
{-# language DeriveFunctor, InstanceSigs, DeriveFoldable #-}

import Control.Monad
import Control.Applicative
import Data.Char

-- Köv órai feladat:
-- (feladatba bemásolva Parser library)
--  satisfy, char, string, <|>, do (vagy Monad műveletek)
--  felhasználásával, valamilyen parser definíció
-- (egyszerű definíció)

-- Feladat
--------------------------------------------------------------------------------


-- cseréljük meg a mezőket, adjuk vissza az új első mező értékét
-- (régi második).

f :: State (a, a) a
f = do
  (a1, a2) <- get
  put (a2, a1)
  return a2

  -- modify (\(a1, a2) -> (a2, a1))
  -- fst <$> get                       -- fmap fst get


-- State monád
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

------------------------------------------------------------

-- Definiálj egy műveletet, ami a lista állapotot kiegészíti egy elemmel
push :: a -> State [a] ()
push a = modify (a:)
    -- modify (\as -> a : as)

-- példák:
-- runState (push 10) [] == ((), [10])
-- runState (push 10 >> push 10 >> push 20) [] == ((), [20, 10, 10])

-- Ha az állapot lista nem üres, akkor a következő függvény leveszi az első
-- elemet és visszaadja Just értékként, egyébként Nothing-ot ad.

-- dropS :: Int -> State [a] ()
-- dropS n = modify (drop n)

pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> return Nothing
    a:as -> put as >> return (Just a)  -- "leveszünk": visszaírjuk a rövidebb listát

-- példák:
-- runState pop []        == (Nothing, [])
-- runState pop [0, 1, 2] == (Just 0, [1, 2])


-- Írj egy függvényt, ami egy Int listában minden elemet kicserél az elemtől
-- balra levő elemek maximumára (beleértve az elemet). Legyen az állapot
-- Nothing, ha még egy értéket sem jártunk be, egyébként pedig Just-ban tárolva
-- az eddigi értékek maximuma.

maxs :: [Int] -> State (Maybe Int) [Int]
maxs = undefined

-- példák:
-- evalState (maxs [1, 2, 5, 2]) Nothing == [1, 2, 5, 5]
-- evalState (maxs [10, 5, 12, 3]) Nothing == [10, 10, 12, 12]

-- pop  :: State [a] (Maybe a)
-- push :: a -> State [a] ()

-- Írj egy függvényt, ami kizárólag push, pop és rekurzió felhasználásával
-- map-eli az állapot listát.
mapPushPop :: (a -> a) -> State [a] ()
mapPushPop = undefined

-- példák:
-- execState (mapPushPop (+10)) [0, 1, 2] == [10, 11, 12]


-- Definiálj egy függvényt, ami kicseréli egy fa leveleiben tárolt értékeket
-- balról jobbra haladva egy megadott lista elemeire. Használj State monádot!

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Functor, Show, Foldable)

-- pl: replaceLeaves [10, 20, 30] (   Node (Leaf 2) (Leaf 3))
--                                 == Node (Leaf 10) (Leaf 20)
--     replacereplaceLeaves [5] (Leaf 10) == Leaf 5
--     replacereplaceLeaves [5]
--        (Node (Leaf 0) (Node (Leaf 0) (Leaf 0))) ==
--        (Node (Leaf 5) (Node (Leaf 0) (Leaf 0)))

replaceLeaves :: [a] -> Tree a -> Tree a
replaceLeaves as t = evalState (go t) as where   -- "inicializáljuk"

  -- go definíciójában, tudunk egy "[a]" típusú értéket írni/olvasni
  go :: Tree a -> State [a] (Tree a)
  go (Leaf a) = do
    ma <- pop  -- mellékhatása: nemüres listából levesz egy értéket
    case ma of
      -- üres volt a lista
      Nothing -> return (Leaf a)
      -- nemüres volt a lista
      Just a' -> return (Leaf a')
  go (Node l r) = do
    l' <- go l
    r' <- go r
    return (Node l' r')

-- imperatív
{-
def f(as, t) :=
   mutvar list := copy(as);
   def go(t) := case t of
     Leaf a := case pop(list) of
       Nothing  -> return (Leaf a)
       Just(a') -> return (Leaf a')
     Node(l, r) -> return Node(go l, go r)
   return go(t);
-}

-- class Foldable t where
--   foldr :: (a -> b -> b) -> b -> t a -> b

-- deriving (Foldable)        -- megkapom a foldr-t

-- foldr :: (a -> b -> b) -> b -> [a]    -> b
-- foldr :: (a -> b -> b) -> b -> Tree a -> b
treeToList :: Tree a -> [a]
treeToList t = foldr (:) [] t
   -- (:)-al kombinálok minden értéket, kiindulva []-el

-- Definiáld a függvényt, ami megfordítja a fa leveleiben tárolt értékek
-- sorrendjét!  tipp: használhatod a replaceLeaves függvényt.

reverseElems :: Tree a -> Tree a
reverseElems t =

   -- let elems = treeToList t
   -- in replaceLeaves (reverse elems) t

   replaceLeaves (reverse elems) t where
     elems = treeToList t


-- + Házi feladat: Notes07.hs fennmaradó State feladatai!
--                 és az itteni State feladatok.

--------------------------------------------------------------------------------
-- Parser, regex-ek
--------------------------------------------------------------------------------


{-
Parser monád használata:

típus:
  f :: Parser a     "f" egy olyan függvény, ami String-ből "a" típusú
                    értéket próbál kiolvasni (String elejéről).
futtatás:
                         input String    (hibázhat) (érték, fennmaradó String)
  runParser :: Parser a ->  String     ->  Maybe    (a, String)

példa:
  parseInt :: Parser Int   -- String elejéről Int literált próbál olvasni

  runParser parseInt :: String -> Maybe (Int, String)

  runParser parseInt "12333xxx" == Just (12333, "xxx")

használjuk:
  - instance-okat: Functor, Monad, Applicative
  - primitív függvények + "kombinátorok"
  - kis parser függvényekből összerakunk bonyolultabbakat

-}

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure

  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

-- eof ("end of file")
--   illeszkedik az üres String-re
-- példa:  runParser eof ""    == Just ((), "")
--         runParser eof "foo" == Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- Egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel.
-- Példa:
--    satisfy (\c -> 'a' <= c && c <= 'z') :: Parser Char

lowercaseChar :: Parser Char
lowercaseChar = satisfy (\c -> 'a' <= c && c <= 'z')

-- runParser lowercaseChar "abc" == Just ('a', "bc")

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- Olvassunk egy konkrét karaktert

-- char 'x' :: Parser ()    -- csak az 'x' karaktert olvassa sikeresen
-- runParser (char 'x') "xy" == Just ((), "y")

char :: Char -> Parser ()
char c = () <$ satisfy (==c)


instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- (<|>) ("choice", "vagy" operátor)
-- p1 :: Parser a
-- p2 :: Parser a
-- p1 <|> p2 :: Parser a    -- ha p1 hibázik, akkor p2-t futtatjuk

myParser :: Parser Char
myParser = lowercaseChar <|> satisfy (\c -> '0' <= c && c <= '9')

  -- runParser myParser "axxx" = Just ('a', "xxx")
  -- runParser myParser "8a"   = Just ('8', "a")
  -- runParser myParser "A"    = Nothing

-- konkrét String olvasása input elejéről:
--   runParser (string "foo") "foobar" == Just ((), "bar")

-- Functor, Monad instance:

-- Egymás utáni végrehajtás Monad műveletekkel:
--   hiba propagálódik + a String változása
myParser2 :: Parser Char
myParser2 = do
  _ <- myParser  -- myParser olvas egy lowercase betűt vagy egy szám karaktert
  c <- myParser
  return c

-- runParser myParser2 "a2xx" == Just ('2',"xxx")

-- -- alkalmazzunk egy függvényt a visszatérési értékre:
-- fmap :: (a -> b) -> Parser a -> Parser b

-- (ord :: Char -> Int) megadja a karaketr kódját (ASCII/Unicode)
-- fmap ord lowercaseChar :: Parser Int
-- runParser (fmap ord lowercaseChar) "axx" == Just (97, "xx")

string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

-- ismételt futtatás (iteráció)
--   many  :: Parser a -> Parser [a]   -- 0-szor vagy többször futtat
--                                     -- egy parser-t
--   many_ :: Parser a -> Parser ()    -- mint many, de ()-ot ad
--   some  :: Parser a -> Parser [a]   -- 1-szer vagy többször futtat
--   some_ :: Parser a -> Parser ()    -- mint some, de ()-ot ad

-- Példák:
--   many lowercaseChar :: Parser [Char]
--   runParser (many lowercaseChar) "slfkjsdlfdsj" == Just ("slfkjsdlfdsj", "")
--   runParser (many lowercaseChar) "aaa999" == Just ("aaa", "999")
--   runParser (many lowercaseChar) "999"    == Just ([], "999")
--   runParser (some lowercaseChar) "999"    == Nothing

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa


------------------------------------------------------------
-- regex-ek: típus Parser ()  (arra vagyunk kíváncsiak, hogy illeszkedik-e
--                             egy regex az inputra)
--
--  Parser ()            regex
--------------------------------
--   char c               c
--  p1 >> p2             p1p2
--  p1 <|> p2            p1|p2
--   many_ p              p*
--   some_ p              p+
--    eof                 $

----------------------------------------

-- Definiáljuk a regex-ekből ismerős ? operátort
optional :: Parser a -> Parser (Maybe a)
optional p =
  --  egyszer sikerül p-t futtatni      egyébként: visszaadjuk a Nothing-ot
  do {a <- p; return (Just a)}      <|>   return Nothing

  -- fmap Just p <|> return Nothing
  -- (Just <$> p) <|> return Nothing


-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)


--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket Parser-ként! Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
-- példák:   kutya fookutya barfookutya
p1 :: Parser ()
p1 = undefined

-- \[foo(, foo)*\]     (nemüres ,-vel választott "foo" lista)
-- példák:   [foo] [foo, foo] [foo, foo, foo]
p2 :: Parser ()
p2 = undefined

-- (ac|bd)*
-- példák:   acac  acbdbd
p3 :: Parser ()
p3 = undefined

-- [a..z]+@foobar\.(com|org|hu)
-- példák:   kutya@foobar.org  macska@foobar.org
p4 :: Parser ()
p4 = undefined

-- -?[0..9]+
-- (?e azt jelenti, hogy e opcionális)
p5 :: Parser ()
p5 = undefined

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p6 :: Parser ()
p6 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példák:  foo=10,bar=30,baz=40
p7 :: Parser ()
p7 = undefined
