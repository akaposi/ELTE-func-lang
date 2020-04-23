
import Control.Monad

-- Feladat 1: vegyük a következő típust, ami a State és Maybe monádok kombinációja
-- A cél az, hogy az (SM s) monádban elérhetők legyenek a State műveletek (get,
-- put, modify) és a hibadobás lehetősége is.
newtype SM s a = SM {runSM :: s -> Maybe (a, s)}
   -- deriving Functor

-- Definiáljuk a Functor instance-t.
instance Functor (SM s) where
  fmap f (SM g) = SM $ \s ->
       fmap (\(a, s') -> (f a, s')) (g s)
    -- case g s of
    --   Just (a, s) -> Just (f a, s)
    --   Nothing     -> Nothing

instance Applicative (SM s) where
  pure = return
  (<*>) = ap

-- Definiáljuk a következő műveletet, ami hibát dob a Nothing felhasználásával!
-- Legyen a működés a következő: legyen bármilyen (sm :: SM s a) és (s :: s) értékre
-- runSM sm s == Nothing
nothing :: SM s a
nothing = SM $ \_ -> Nothing

-- Definiáljuk a catch függvényt, amely lehetőséget ad a futás folytatására
-- hiba esetén.
-- Működés: futassuk az első paraméter műveletet, ha Nothing-al tér vissza, akkor
-- futtassuk a második paramétert.
-- Azaz:  runSM (catch nothing sm) s == runSM sm s     bármely sm, s-re
--        runSM (catch sm1 sm2) s    == runSM sm1 s,   ha  (runSM sm1 s /= Nothing)

catch :: SM s a -> SM s a -> SM s a
catch (SM f) (SM g) = SM $ \s -> case f s of
  Nothing -> g s
  x       -> x

-- definiáljuk a get/put műveleteket!
-- ugyanaz mint State monádban
get :: SM s s
get = SM $ \s -> Just (s, s)

put :: s -> SM s ()
put s = SM $ \_ -> Just ((), s)

-- Definiáljuk a Monad instance-t!
instance Monad (SM s) where
  return a = SM $ \s -> Just (a, s)  -- State return + Maybe return
  SM f >>= g = SM $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runSM (g a) s'
   -- f s >>= \(a, s') -> runSM (g a) s'

--
--------------------------------------------------------------------------------

type Parser a = SM String a
--   olyan függvény: String -> Maybe (a, String)
--                 input   ->     (kiolvasott érték, háralevő String)

-- kiindulunk "elemi" parser függvényekből
--   magasabbrendű függvényekkel összerakunk nagyobb parsereket

-- definiáljuk a regexeknek megfelelő elemi parsereket
-- felismerés: Parser ()

-- regex: üres regex, atomi szimbolum regex, konkatenáció, választás (|), *

-- Adott karaktert olvasunk egy String elejéről (atomi regex)
char :: Char -> Parser ()
char c = SM $ \s -> case s of
  []    -> Nothing
  c':cs -> if c == c' then Just ((), cs) else Nothing

-- Üres regex
eof :: Parser ()
eof = SM $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- konkatenáció: (>>)

-- választás:
pab :: Parser ()
pab = catch (char 'a') (char 'b')

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = catch

p1 :: Parser ()
p1 = (char 'f' >> char 'o' >> char 'o') <|>
     (char 'b' >> char 'a' >> char 'r')

p1' :: Parser ()   -- illeszt az input végére
p1' = p1 >> eof

string :: String -> Parser ()
string = mapM_ char
-- string []     = pure ()
-- string (c:cs) = char c >> string cs

p1'' :: Parser ()
p1'' = string "foo" <|> string "bar"

-- * (iteráció)

-- kölcsönös definíció: + és *
-- std modul: Control.Applicative
--   many : _* regex
--   some : _+ regex

many :: Parser a -> Parser [a]   -- 0 vagy több eredmény gyűjtve egy listába
many pa = some pa <|> pure []

some :: Parser a -> Parser [a]   -- 1 vagy több eredmény gyűjtve egy listába
some pa = (:) <$> pa <*> many pa
  -- do
  --   a  <- pa
  --   as <- many pa
  --   pure (a:as)

-- Control.Applicative-beli some és many : előadás jegyzetben

-- példa
--   runSM (some (char 'x')) "xxxxx" == Just ([(),(),(),(),()],"")

-- ezen ponton: lehetséges feladat: írj regexeket


-- környezetfüggő parserek: bind-olás szükséges hozzá
------------------------------------------------------------

-- klassszikus példa: n-darab 'a' után n-darab 'b'

p2 :: Parser ()
p2 = do
  as <- many (char 'a')
  replicateM_ (length as) (char 'b')  -- Control.Monad.replicateM_
                                      -- végrehajt valamit n-szer

-- feladat: kiegyensúlyozott zárójelezést
--   példák: ()     OK
--           (())   OK
--           ()()   OK
--           ()(()) OK
--           (()    Nem OK

-- tipp: <|>, >>, char, pure,
--       rekurzió is szükséges lesz

par :: Parser ()
par = many (char '(' >> par >> char ')') >> pure ()

-- házi feladat: par, viszont ne olvassa az üres stringet!

par' :: Parser ()
par' = par >> eof
