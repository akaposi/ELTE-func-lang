
{-# language InstanceSigs, DeriveFunctor #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char  -- isDigit, isAlpha, digitToInt

--------------------------------------------------------------------------------
-- Köv feladat:
-- Regex definiálása parser-el
--------------------------------------------------------------------------------

data List a = Nil | Cons1 a (List a) | Cons2 a a (List a)
  deriving (Eq, Show)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f b Nil = b
  foldr f b (Cons1 a as) = f a (foldr f b as)
  foldr f b (Cons2 a1 a2 as) = f a1 (f a2 (foldr f b as))
  -- foldr (:) []


-- Parser library
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

-- f :: Parser a
--   String-ből "a" típusú adatot kiolvasó függvény

--                            input String      Maybe (érték, output String)
--   runParser :: Parser a -> String         -> Maybe (a, String)

-- f :: Parser a
-- futattás:
--    runParser f :: String -> Maybe (a, String)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (<*>) = ap

instance Monad Parser where
  return = pure

  -- egymás után olvasás,
  -- pl: p1 >> p2           (p1 olvas valahány karaktert, utána p2 a maradékból)
  --                        (p1 hibáz, akkor p2 már nem fut)
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing     -> Nothing
    Just (a, s) -> runParser (g a) s

-- "kombinátor" library
--   stílus: kis parser függvényekből építünk összetett függvényeket

-- "end-of-file":   akkor sikeres, ha üres az input
eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- pl: runParser eof "" == Just ((), "")
--     runParser eof "foo" == Nothing

-- Egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- runParser (char 'x') "xyy" == Just ((), "yy")
-- runParser (char 'x') "y"   == Nothing
-- runParser (char 'x' >> char 'y' >> char 'z') "xyz" == Just ((), "")

instance Alternative Parser where

  -- parser ami azonnal hibázik
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  -- "choice": választás két parser között.
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

  -- példa:
  -- runParser (char 'x' <|> char 'y') "xfoo" == Just ((), "foo")
  -- runParser (char 'x' <|> char 'y') "yfoo" == Just ((), "foo")


-- konkrét String olvasása:
--   egy String minden karakterét olvassuk sorban
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- runParser (string "foo") "foobar" == Just ((), "bar")

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

-- many pa : futtatja pa-t 0-szor vagy többször
--           listában visszaadja az összes eredményt

-- satisfy isLetter        :: Parser Char
-- many (satisfy isLetter) :: Parser String

-- runParser (many (satisfy isLetter)) "xxx3334"
--   == Just ("xxx","3334")

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  --   -- infix operátor: (<$>)

  --  kicseréli egy művelet visszatérési értékét egy konkrét értékre
  -- (<$) :: Functor f => b -> f a -> f b
  -- (<$) b fa = fmap (\_ -> b) fa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

-- standard: Control.Applicative.optional
optional' :: Parser a -> Parser (Maybe a)
optional' pa = (Just <$> pa) <|> pure Nothing
   -- (<$>) az fmap infix formája

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional' pa

-- Összehasonlítás: regex-ek
------------------------------------------------------------

-- egyszerű regex: Parser ()
--  (nem ad vissza értéket, csak azt mondja meg hogy illeszkedik-e a
--   parser az inputra)

--   c           char c
--   ^$          eof
--   r₁r₂        r₁ >> r₂
--   r₁|r₂       r₁ <|> r₂
--   r*          many_ r
--   r+          some_ r
--   r?          optional_ r

------------------------------------------------------------

-- olvassunk 0 vagy több pa-t, psep-el elválasztva
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

-- olvassunk 1 vagy több pa-t, psep-el elválasztva
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = (:) <$> pa <*> many (psep *> pa)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

------------------------------------------------------------

-- pozitív Int olvasása
pPos :: Parser Int
pPos = do
  ds <- some pDigit
  pure $ sum $ zipWith (*) (reverse ds) (iterate (*10) 1)

rightAssoc :: (a -> a -> a) -> Parser a            -> Parser sep -> Parser a
rightAssoc f pa psep = foldr1 f <$> sepBy1 pa psep

leftAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
leftAssoc f pa psep = foldl1 f <$> sepBy1 pa psep

nonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
nonAssoc f pa psep = do
  exps <- sepBy1 pa psep
  case exps of
    [e]      -> pure e
    [e1,e2]  -> pure (f e1 e2)
    _        -> empty

-- nem láncolható prefix operátor
prefix :: (a -> a) -> Parser a -> Parser op -> Parser a
prefix f pa pop = (pop *> (f <$> pa)) <|> pa


--------------------------------------------------------------------------------

-- Implementáld a következő regex-eket Parser-ként! Szükség szerint definiálj
-- segédfüggvényeket.

-- (foo|bar)*kutya
-- példák:   kutya fookutya barfookutya
p1 :: Parser ()
p1 = many_ (string "foo" <|> string "bar") >> string "kutya"

-- \[foo(, foo)*\]     (nemüres ,-vel választott "foo" lista)
-- példák:   [foo] [foo, foo] [foo, foo, foo]
p2 :: Parser ()
p2 = string "[foo" >> many_ (string ", foo") >> char ']'

-- ugyanaz do-val:
p2' :: Parser ()
p2' = do
  string "[foo"
  many_ (string ", foo")
  char ']'

-- (ac|bd)*
-- példák:   acac  acbdbd
p3 :: Parser ()
p3 = many_ (string "ac" <|> string "bd")

-- regex:   [c₁..c₂]         library:   charRange_ c₁ c₂

charRange :: Char -> Char -> Parser Char
charRange c1 c2 = satisfy (\c -> c1 <= c && c <= c2)

charRange_ :: Char -> Char -> Parser ()
charRange_ c1 c2 = () <$ charRange c1 c2

-- [a..z]+@foobar\.(com|org|hu)
-- példák:   kutya@foobar.org  macska@foobar.org
p4 :: Parser ()
p4 = some_ (charRange 'a' 'z') >>   -- "do" is lehet >> helyett it is
     string "@foobar." >>
     (string "com" <|> string "org" <|> string "hu")

pDigit_ :: Parser ()
pDigit_ = charRange_ '0' '9'    -- vagy: satisfy isDigit

-- -?[0..9]+
-- (?e azt jelenti, hogy e opcionális)
p5 :: Parser ()
p5 = do
  optional_ (char '-')
  some_ pDigit

pLowerCase_ = charRange_ 'a' 'z'
pUpperCase_ = charRange_ 'A' 'Z'
pLetter_    = pLowerCase_ <|> pUpperCase_

-- ([a..z]|[A..Z])([a..z]|[A..Z]|[0..9])*
p6 :: Parser ()
p6 = pLetter_ >> many_ (pLetter_ <|> pDigit_)

-- ([a..z]+=[0..9]+)(,[a..z]+=[0..9]+)*
-- példák:  foo=10,bar=30,baz=40

-- p7 :: Parser ()
-- p7 = do
--   (some_ pLowerCase_ >> char '=' >> some_ pDigit_)
--   many_ (char ',' >> some_ pLowerCase_ >> char '=' >> some_ pDigit_)

--
p7 :: Parser ()
p7 = () <$ sepBy1 (some_ pLowerCase_ >> char '=' >> some_ pDigit_) (char ',')
                 --           ismételt parser                      szeparátor

--------------------------------------------------------------------------------

-- Olvass be (Int, Int, Int) típusú kifejezéseket Haskell szintaxis szerint!
-- engedj meg mindenhol whitespace karaktereket! Az Int-ek legyenek csak pozitívok,
-- olvasd be őket a pPos segítségével.
pTripleInt :: Parser (Int, Int, Int)
pTripleInt = undefined

-- Hasonló módon olvass (Maybe (Int, Int)) értékeket.
pMaybePair :: Parser (Maybe (Int, Int))
pMaybePair = undefined


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


-- Írj egy parser-t, ami zárójeleket, +-t, *-t és pozitív Int literálokat tartalmazó
-- kifejezéseket olvas! Whitespace-t mindenhol engedj meg.
--------------------------------------------------------------------------------
--
--   példák: 10 + 20 + 30
--           (10 + 20) + 30
--           10 + ((20 + 5))
--           10 * 20 + 5
--
--
-- A + operátor jobbra asszociáljon, azaz pl. 1 + 2 + 3 olvasása legyen
--  (Plus (Lit 1) (Plus (Lit 2) (Lit 3))).
-- A * operátor szintán jobbra asszociáljon, és kössön erősebben, mint a +.

data Exp = Lit Int | Plus Exp Exp | Mul Exp Exp deriving Show

pExp :: Parser Exp
pExp = undefined


-- bónusz : írj parser-t típusozatlan lambda kalkulushoz! (whitespace megengedett)
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
