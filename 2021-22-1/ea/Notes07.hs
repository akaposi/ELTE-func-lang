{-# language InstanceSigs #-}
import Control.Monad
import Control.Applicative -- class Alternative
import Data.Foldable  -- traverse_

-- opcionális 1. házi: ezen a héten felteszem
--    nagyobb State monad feladat (interpreter)

-- canvas-ba kell beküldeni egy fájlban
--   (van hozzá tesztsor)

-- Bevezetés: Parser monád
------------------------------------------------------------

-- parser programok: bemenet String / karaktersorozat
--    (lényeg: primitív adatsorozat --> strukturált adat)
--    (nyelvek parsolása: string --> szintaxisfa)

-- félév során: anyag célja: parser + interpreter Haskell-ben
--     monád/Applicative felhasználásával

-- mi egy parser függvény?

--      input     hiba  ( strukturált érték, hátramardó input )
-- f :: String -> Maybe (      a           ,  String          )

-- parser: State String + hiba lehetősége
--         State String + Maybe    ( lásd monád transzformerek:
--                                   automatikusan ugyanez )

-- Hibaüzenetek: csak Nothing
--    - lehet tovább fejleszteni az implementációt
--    - jobb hibaüzenetek

-- Parser a : parser függvény, ami "a" típusú
--            értéket próbál olvasni egy String elejéről.
newtype Parser a =
  Parser {runParser :: String -> Maybe (a, String)}

-- lehetne (deriving Functor is)
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing      -> Nothing
    Just (a, s') -> Just (f a, s')

  -- Functor Maybe használatával:
  -- fmap f (Parser g) = Parser $ \s ->
  --   fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where

  -- nem dob hibát + nem fogyaszt inputot
  return :: a -> Parser a
  return a = Parser $ \s -> Just (a, s)

  -- egymás után két parsert hívunk
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'
       -- g               :: a -> Parser b
       -- g a             :: Parser b
       -- runParser (g a) :: String -> Maybe (b, String)

-- Alapvető parserek + "kombinátorok"
--   ("parser combinator" library)
--   alapvető parserek + magasabbrend függvények
--        (egy vagy több Parser-ból újabb Parser-t ad)

-- "end-of-file", akkor sikeres, ha az input üres
-- standard elnevezés gyakran használt library-kben
-- regex: $
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- ha nemüres az input & egy feltétel igaz az első karakterre,
-- akkor sikeres, és visszaadja a Char-t.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:s | f c -> Just (c, s)   -- output String 1-el rövidebb!
  _         -> Nothing

-- olvassunk egy konkrét karaktert
char :: Char -> Parser ()
char c = () <$ satisfy (==c)

-- std: olvassunk bármilyen karaktert
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

   -- fmap (\_ -> ()) :: Functor f => f a -> f ()
   -- (<$) :: Functor f => a -> f b -> f a
   -- (<$) a fa = fmap (\_ -> a) fa

-- három karakter olvasás egymás után
p1 = char 'x' >> char 'y' >> char 'z'
-- runParser p1 "xyzzzzz" == Just ((),"zzzz")
-- runParser p1 "xyx" == Nothing

-- kizárólag "abc" String-et fogad el
p2 = char 'a' >> char 'b' >> char 'c' >> eof

-- konkrét String-et olvasó parser
string :: String -> Parser ()
string = traverse_ char
    -- () <$ traverse char : kevésbé hatékony, [()] listát, utána
    --                       eldobjuk

-- több parser közötti választás

-- konvenció:
--   - osztály: Alternative
--   - metódusok: empty, (<|>)
--   - instance Alternative Parser

-- class Applicative f => Alternative f where
--   empty :: f a
--   (<|>) :: f a -> f a -> f a

-- olyan Applicative f-ek, amik minden (f a)-ra Monoid
-- Monoid változata, 1-paraméteres típusokra

instance Alternative Parser where

   -- rögtön hibát dobó parser
   empty :: Parser a
   empty = Parser $ \_ -> Nothing

   -- próbáljuk az elsőt olvasni, ha az hibázik, akkor a másodikat
   (<|>) :: Parser a -> Parser a -> Parser a
   (<|>) (Parser f) (Parser g) = Parser $ \s ->
     case f s of Nothing -> g s
                 x       -> x

p3 = string "kutya" <|> string "macska"

-- iterálás:
-- Control.Applicative:
--
--    1-szer vagy többször próbál olvasni egy parser-t
--    some :: Alternative f => f a -> f [a]

--    0-szor vagy többször próbál olvasni
--    many :: Alternative f => f a -> f [a]

p4 = many (string "kutya" <|> string "macska")

-- runParser p4 "kutyamacskakutyaxxxxxxx"
--   == Just ([(),(),()],"xxxxxxx")

-- runParser p4 "xxxx" == Just ([],"xxxx")

-- std definíció: rekurzív
many' :: Parser a -> Parser [a]
many' pa = some' pa <|> pure []

some' :: Parser a -> Parser [a]
some' pa = (:) <$> pa <*> many' pa
   -- do {a <- pa; as <- many' pa; pure (a:as)}

-- kölcsönös rekurzió nélkül:
-- many' :: Parser a -> Parser [a]
-- many' pa = ((:) <$> pa <*> many' pa) <|> pure []

-- eddig: regex-eket támogatjuk:
--   üres, konkatenáció, választás, szimbólum, *

-- tetszőleges (egyszerű) regex megírható fenti függvényekkel

-- példa: nem írható meg regex-el
   -- mivel Monad instance + akármilyen rekurzió
   -- (library: Turing teljes rekurzív leszállás)
           -- regex is egy számítási modell:
             -- komplexitás: lineáris, mindig terminál
             -- (olyan parser algo-k, amik szintén mindig terminálnak)
             -- ...
             -- (tesztőleges programok: akármilyen lassú lehet,
             --  nem feltételül terminál)
             --  (tetszőleges programok hagyományos def:
             --   Turing gépek)

  -- "rekurzív leszállás" : hagyományos elnevezése
  --   az olyan parsereknek, amelyek rekurzió + <|> segítségével
  --   vannak implementálva
  -- (recursive descent)

  -- (van még más parser algoritmus család is)

-- példa: parser ami nem regex

-- olvas N-darab 'a' karaktert, utána pedig *ugyanannyi*
-- darab 'b' karaktert.
--   "", "ab", "aabb", "aaabbb", ...
p5 :: Parser ()
p5 = do
  s <- many (char 'a')
  let n = length s
  replicateM_ n (char 'b')
-- "környezetfüggő" parser

-- Applicative parser : környezetfüggetlen
-- Monadikus   parser : környezetfüggő (is lehet)


-- További kombinátorok

-- olvassunk valami előtt és után egy adott parser-t
between :: Parser around -> Parser a -> Parser a
between around pa = do
  around
  a <- pa
  around
  pure a

-- tegyük zárójelbe a parser-t
parens :: Parser a -> Parser a
parens pa = do
  char '('
  a <- pa
  char ')'
  pure a

-- runParser (parens (string "Kutya")) "(Kutya)xxxx"
--   == Just ((),"xxxx")

-- Applicative stílusban: (regex-ekhez és nyelvtan definíciókhoz
--                         áll közelebb)

between' :: Parser around -> Parser a -> Parser a
between' around pa = around *> pa <* around

  -- Applicative kombinátorok:
  --   (<$>)
  --   (<*>)
  --   (<$)
  --   (<*)
  --   (*>)

  -- egymás után végrehajtuk két műveletet, visszaadjuk
  -- az első értékét
  -- (<*) :: Applicative f => f a -> f b -> f a

  -- egymás után végrehajtuk két műveletet, visszaadjuk
  -- a második értékét
  -- (*>) :: Applicative f => f a -> f b -> f b

  -- amire a csőrök mutatnak, annak a műveletnek
  -- az értékét adjuk vissza

parens' :: Parser a -> Parser a
parens' pa = char '(' *> pa <* char ')'

-- Applicative
p6 :: Parser String
p6 = char 'x' *> many (satisfy (/='x')) <* char 'x' <* char 'y'

  -- runParser p6 "xfooobarxy" == Just ("fooobar","")

-- monádikus
p6' :: Parser String
p6' = do
  char 'x'
  s <- many (satisfy (/='x'))
  char 'x'
  char 'y'
  pure s
