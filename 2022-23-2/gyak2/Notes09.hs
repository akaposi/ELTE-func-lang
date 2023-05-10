
{-# language DeriveFunctor, InstanceSigs, DeriveFoldable #-}

import Control.Monad
import Control.Applicative
import Data.Char

-- Következő feladat: regex (közepesen bonyolult)
--                    mint itt p1-p7

-- Megoldás
--------------------------------------------------------------------------------

f :: Parser ()
f = string "kutya" <|> string "macska" <|> string "béka"

--------------------------------------------------------------------------------

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

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- (p1 <|> p2) először p1-et futtatja, ha az hibázik, akkor p2-t.
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- Sikeres az üres inputon
eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- Olvassunk egy karaktert amire igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- Olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- Olvassunk egy konkrét string-et az input elejéről
string :: String -> Parser ()
string = mapM_ char

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

-- Ismétlés:
--------------------------------------------------------------------------------

-- runParser :: Parser a -> String -> Maybe (a, String)


-- char :: Char -> Parser ()                 -- konkrét karakter olvasása
-- string :: String -> Parser ()             -- konkrét String olvasása
-- eof :: Parser ()                          -- üres input olvasása
-- (<|>) :: Parser a -> Parser a -> Parser a -- választás ("vagy")
-- many :: Parser a -> Parser [a]            -- 0-szor vagy többször olvasás
-- some :: Parser a -> Parser [a]            -- 1-szer vagy többször olvasás
-- many_ :: Parser a -> Parser ()
-- some_ :: Parser a -> Parser ()
-- satisfy :: (Char -> Bool) -> Parser Char  -- 1 karakter olvasása amire
                                             -- igaz a feltétel

-- Functor instance
--   fmap :: (a -> b) -> Parser a -> Parser b

-- Monad instance
--   return :: a -> Parser a
--   (>>=) :: Parser a -> (a -> Parser b) -> Parser b

-- írj egy Parser-t, ami 0 vagy több 'A' karaktert olvas, és
-- visszaadja, hogy hányat olvasott (mint Int)
countAs :: Parser Int
countAs = length <$> many (char 'A')

countAs' :: Parser Int
countAs' = do            -- fmap nélkül, Monad instance-al?
  cs <- many (char 'A')
  return (length cs)

------------------------------------------------------------

-- regex-ek: típus Parser ()  (arra vagyunk kíváncsiak, hogy illeszkedik-e
--                             egy regex az inputra)
--
--  Parser ()            regex
--------------------------------
--   char c               c
--   string s             s      (karaktersorozat)
--  p1 >> p2             p1p2
--  p1 <|> p2            p1|p2
--   many_ p              p*
--   some_ p              p+
--    eof                 $

--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket Parser-ként! Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
-- példák:   kutya fookutya barfookutya
p1 :: Parser ()
p1 = many_ (string "foo" <|> string "bar") >> string "kutya"
  -- (OverloadedStrings)

-- \[foo(, foo)*\]     (nemüres ,-vel választott "foo" lista)
-- példák:   [foo] [foo, foo] [foo, foo, foo]
p2 :: Parser ()
p2 = char '[' >> string "foo" >> many_ (string ", foo")
     >> char ']'

-- x <$ p        cseréljük ki "p" visszatérési értékét
--               "x"-re

p2' :: Parser ()
p2' = char '[' >> sepBy1 (string "foo") (string ", ") >>
      char ']'

p2'' :: Parser ()
p2'' = do
  char '['
  sepBy1 (string "foo") (string ", ")
  char ']'

-- (ac|bd)*
-- példák:   acac  acbdbd
p3 :: Parser ()
p3 = many_ (string "ac" <|> string "bd")

lowercase_ :: Parser ()
lowercase_ = () <$ satisfy isLower
    -- kicserélem a visszatérési értéket ()-ra

-- [a..z]+@foobar\.(com|org|hu)
-- példák:   kutya@foobar.org  macska@foobar.org
p4 :: Parser ()
p4 = some_ lowercase_ >> string "@foobar." >>
     (string "com" <|> string "org" <|> string "hu")

-- -?[0..9]+
-- (e? azt jelenti, hogy e opcionális)
p5 :: Parser ()
p5 = undefined

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p6 :: Parser ()
p6 = undefined

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példák:  foo=10,bar=30,baz=40
p7 :: Parser ()
p7 = undefined


-- Whitespace, strukturált adat olvasás
------------------------------------------------------------

-- Whitespace-ek szerepelhetnek bárhol
-- Hogyan kezeljük:
--   1. Definiáljuk "ws"-t, ami whitespace-t olvas
--   2. Minden alapvető parser (ami ténylegesen karaktert olvas)
--      maga után hívja meg ws-t
--   3. Cél parser ("top-level parser") futtatása:
--      topLevel kombinátor segítségével

topLevel :: Parser a -> Parser a
topLevel pa = ws *> pa <* eof

ws :: Parser ()
ws = many_ (char ' ' <|> char '\n')

char' :: Char -> Parser ()
char' c = char c >> ws

string'  :: String -> Parser ()
string' s = string s >> ws

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = satisfy f <* ws

   --  p1 <* p2    : végrehajtjuk p1 után p2-t, p1 értékét
   --                adjuk vissza

   --  p1 *> p2    : végrehajtjuk p1 után p2-t, p2 értékét
   --                adjuk vissza

   -- p1 *> p2 <* p3 <* p4 <* p5

   -- do  x1 <- p1
   --     x2 <- p2
   --     x3 <- p3
   --     return x2

   -- p1 *> p2 <* p3

pPos' :: Parser Int
pPos' = pPos <* ws

-- Olvass be (Int, Int, Int) típusú kifejezéseket Haskell szintaxis szerint!
-- engedj meg mindenhol whitespace karaktereket! Az Int-ek legyenek csak pozitívok,
-- olvasd be őket a pPos segítségével.
pTripleInt :: Parser (Int, Int, Int)
pTripleInt = topLevel $ do
  char' '('
  n1 <- pPos'
  char' ','
  n2 <- pPos'
  char' ','
  n3 <- pPos'
  char' ')'
  return (n1, n2, n3)

-- Applicative stílusban:
--   (<$>)   (fmap)
--   (<*>)   (ap)
--   (<*)
--   (*>)
--   (<$)
pTripleInt' :: Parser (Int, Int, Int)
pTripleInt' =
  (,,) <$> (char' '(' *> pPos' <* char' ',')
       <*> (pPos' <* char' ',')
       <*> (pPos' <* char' ')')

-- Hasonló módon olvass (Maybe (Int, Int)) értékeket.
pMaybePair :: Parser (Maybe (Int, Int))
pMaybePair = undefined

-- Hasonló módon, Haskell szintaxis szerint olvass [Int] értékeket
pIntList :: Parser [Int]
pIntList = undefined


-- Olvass [[(String, Int)]] típusú adatot a következő szintaxis szerint:
--  - a bemenet 0 vagy több sorból áll
--  - minden sorban legyen vesszővel elválasztva 0 vagy több kulcs-érték pár
--  - egy kulcs-érték pár szintaxisa legyen "kulcs=érték", ahol a kulcs egy nemüres
--    betűsorozat, az érték pedig egy pozitív Int.
--  - whitespace nem megengedett

-- például:
--    bemenet: "x=0,y=1,foo=200\nx=10,y=20"
--    érték:   [[("x", 0), ("y", 1), ("foo", 200)], [("x", 10), ("y", 20)]]
pKeyVals :: Parser [[(String, Int)]]
pKeyVals = undefined


-- Bónusz : írj parser-t típusozatlan lambda kalkulushoz!
-- (whitespace megengedett mindenhol)
--------------------------------------------------------------------------------

-- bemenet-kimenet példák:
--    x           Var "x"
--    y           Var "y"
--  f x           App (Var "f") (Var "x")
--  f x y         App (App (Var "f") (Var "x")) (Var "y")
--  \x. x         Lam "x" (Var "x")
--  \x y. x       Lam "x" (Lam "y" (Var "x"))
--  \x. \y. x     Lam "x" (Lam "y" (Var "x"))
--  (\x. x) y     App (Lam "x" (Var "x")) (Var "y")

data Tm = Var String | App Tm Tm | Lam String Tm deriving Show

pTm :: Parser Tm
pTm = undefined
