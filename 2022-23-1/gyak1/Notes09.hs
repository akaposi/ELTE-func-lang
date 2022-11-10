
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char  -- isDigit, isAlpha, digitToInt


-- Parser library
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

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- egy karaktert olvassunk az input elejéről, amire
-- igaz egy feltétel
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)
  -- satisfy (==c)   hiba: Parser Char helyett Parser () kéne

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- konkrét String olvasása:
string :: String -> Parser ()
string = mapM_ char -- minden karakterre alkalmazzuk a char-t

-- standard függvények (Control.Applicative-ból)
-- many :: Parser a -> Parser [a]
--    (0-szor vagy többször futtatunk egy parser-t)
-- some :: Parser a -> Parser [a]
--    (1-szor vagy többször futtatunk egy parser-t)

many_ :: Parser a -> Parser ()
many_ pa = () <$ many pa

some_ :: Parser a -> Parser ()
some_ pa = () <$ some pa

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
