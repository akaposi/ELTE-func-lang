
-- lista példák (sublists, filter, traverse, applicative)
-- parser Monad, Applicative, regex, függő független, whitespace/token

import Control.Monad
import Control.Applicative

--------------------------------------------------------------------------------

-- példák:

list0 :: [Int]
list0 = (+10) <$> [0..10]

list1 :: [(Int, Int)]
list1 = (,) <$> [0..10] <*> [0..10]

-- (<*>) :: [a -> b] -> [a] -> [b]
-- (<*>) mf ma = do
--   f <- mf
--   a <- ma
--   pure (f a)

list2 :: [Int]
list2 = (\x y z -> x + y + z) <$> [0..5] <*> [0..5] <*> [0..5]

-- traverse listával
-- listát bejárjuk lista Applicative-al
-- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]

list3 :: [[Int]]
list3 = traverse (\x -> [x, x+1]) [0 :: Int, 1, 2, 3]

list4 :: [[Int]]
list4 = traverse (\x -> [x + 10]) [0 :: Int, 1, 2, 3]

-- adjuk vissza egy lista összes lehetséges részlistáját

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (a:as) = let as' = sublists as in map (a:) as' ++ as'

-- minden "a" bele is kerül az eredménybe, meg nem is

sublists' :: [a] -> [[a]]
sublists' = filterM (\_ -> [True, False])      -- gyakorlat: fejtsük ki sublists' definíciót, lássuk be, hogy ugyanaz, mint
                                               -- sublists


-- Parser monád
--------------------------------------------------------------------------------

-- Feladat: parser függvények
--   String-ből --> strukturált adatot (szintaxisfa)
--   String-ből --> Bool (jól formált-e a String, eleme egy adott formális nyelvnek)

-- String -> Maybe a
-- ha több résznyelvet tudunk parsolni, akkor tudjunk összetett nyelvet parsolni
--  (kis parserből nagyot szeretnénk összerakni)

-- String -> Maybe (a, String)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
                             -- Nothing     : nem lehet "a" olvasni
                             -- Just (a, s) : sikeresen olvastunk "a" típusú értéket az input *elejéről* és
                             --               visszadjuk a maradék String-et

-- State + Maybe monád
-- hiba + put + get
-- precízebben: State-el transzformált Maybe (Monad transzformer: segítségével több Monad-ot lehet "stack"-elni)

-- sikeres parse-olás kimenetére egy függvényt alkalmaz
instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing      -> Nothing
    Just (a, s') -> Just (f a, s')

  -- fmap f (Parser g) = Parser $ \s -> fmap (\(a, s) -> (f a, s)) (g s)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
  return a       = Parser $ \s -> Just (a, s)   -- nem olvas inputot, visszaad egy értéket rögtön
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'
      -- g :: a -> Parser b

get :: Parser String      -- jelenlegi input
get = Parser $ \s -> Just (s, s)

put :: String -> Parser ()
put s = Parser $ \_ -> Just ((), s)

-- rögtön hibázó parser
err :: Parser a
err = Parser $ \_ -> Nothing


-- "elemi" parserek
--------------------------------------------------------------------------------

-- "end of file" : üres inputra illeszkedő parser
eof :: Parser ()
eof = do
  s <- get
  case s of
    [] -> pure ()
    _  -> err

-- eof :: Parser ()
-- eof = Parser $ \s -> case s of
--   [] -> Just ((), [])
--   _  -> Nothing

-- Sikeres, ha az input nemüres és egy Char -> Bool feltétel igaz az első karakterre.
-- Siker esetén az első Char-t kiolvassuk és visszaadjuk.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  s <- get
  case s of
    c:s | f c -> do {put s; pure c}
    _         -> err

-- konkrét karakter olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- (const ()) <$> satisfy (==c)
    -- (<$) :: Functor f => a -> f b -> f a     "konstans fmap"
    -- a <$ fb = const a <$> fb

-- példák:
-- egymás után három karakter olvasása:

p1 :: Parser ()
p1 = char 'f' >> char 'o' >> char 'o'

-- futtatás: runParser p1 "foooooooooooooooo" == Just ((),"oooooooooooooo")
--           runParser p1 "macska" == Nothing

p2 :: Parser ()
p2 = do
  char 'f'
  char 'o'
  char 'o'
  eof         -- csak "foo" lehet az input, utána az input vége kell, hogy legyen

-- konkrét String olvasása
string :: String -> Parser ()
string str = mapM_ char str   -- vagy : () <$ traverse char str

-- runParser (replicateM_ 5 (string "foo")) "foofoofoofoofookkkkkk" == Just ((),"kkkkkk")
-- runParser (replicateM_ 5 (string "foo")) "foofoofoofookkkkkk" == Nothing

--------------------------------------------------------------------------------

-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a     -- "choice"

-- Alternative f : f olyan Applicative, minden a-ra Monoid (f a)

-- instance Monoid (Parser a) where
--   mempty = ...
--   (<>)   = ...

instance Alternative Parser where
  empty = err                                          -- (<|>) egységeleme

  -- backtrackelő választás
  Parser f <|> Parser g = Parser $ \s -> case f s of
    Nothing      -> g s
    Just (a, s') -> Just (a, s')

-- <|>, Monad, Applicative + etc, "rekurzív leszálló" parser

--    "kombinátoros"   rekurzív parser
--    nem-kombinátoros rekurzív parser?

-- nem rekurzív parser: "generált"  parser (parser generátorok)
--    input: nyelvtan   output: parser program
--    előny:    gyors program
--    hátrány:  kevésbé kényelmes fejlesztés, nehéz jó hibaüzeneteket adni
--      GCC, Clang, Rust : rekurzív parser implementációk


--------------------------------------------------------------------------------

-- runParser (string "foo" <|> string "bar") "bar" == Just ((), "")

-- regex reprodukciója:
--------------------------------------------------------------------------------

-- iteráció
-- standard függvény: Control.Applicative : some, many

-- 0-szor vagy többször próbál (f a)-t futtatni
many' :: Alternative f => f a -> f [a]
many' fa = ((:) <$> fa <*> many' fa) <|> pure []

-- 1-szer vagy többször
some' :: Alternative f => f a -> f [a]
some' fa = (:) <$> fa <*> many' fa

-- c         char c
-- e₁|e₂     e₁ <|> e₂
-- ε         eof
-- e*        many e

-- runParser (many (string "foo") >> many (string "bar") >> pure ()) "foofoobarbarxxxx"
--   == Just ((),"xxxx")

--------------------------------------------------------------------------------

-- tudunk-e Parser-t írni, ami regex-el lehetetlen?
--    pl környezetfüggő parser

-- olvassuk valahány 'a'-t, utána *ugyanannyi* 'b'-t
-- függőség: Monadikus bind/do

ab :: Parser ()
ab = do
  as <- many (char 'a')
  replicateM_ (length as) (char 'b')    -- do/bind szükséges környezetfüggő parsoláshoz


--------------------------------------------------------------------------------
