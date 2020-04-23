
import Control.Monad       -- replicateM_
import Control.Applicative -- Alternative
import Data.Char           -- isLetter, isDigit, isAlpha

-- Parser monad, parsing
--------------------------------------------------------------------------------

-- Haskell alkalmazások, ahol a parserek lényegesen szerepelnek:
--    pandoc (dokumentum konverter)
--    fordítóprogramok: GHC, Elm, PureScript, Agda, Idris

-- State + Maybe kombinált monád
newtype SM s a = SM {runSM :: s -> Maybe (a, s)}

instance Functor (SM s) where
  fmap f (SM g) = SM $ \s -> fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative (SM s) where
  pure = return
  (<*>) = ap

instance Monad (SM s) where
  return a = SM $ \s -> Just (a, s)     -- State + Maybe return értelemszerű kombinálása
  SM f >>= g = SM $ \s -> f s >>= \(a, s') -> runSM (g a) s'
    -- case f s of
    --   Nothing      -> Nothing
    --   Just (a, s') -> runSM (g a) s'

get :: SM s s
get = SM $ \s -> Just (s, s)

put :: s -> SM s ()
put s = SM $ \_ -> Just ((), s)

modify :: (s -> s) -> SM s ()
modify f = do
  s <- get
  put (f s)


-- hibadobás + hibából való visszatérés:
-- Alternative típusosztály  (Control.Applicative-ben definiálva)

-- class Applicative f => Alternative f where
--   empty :: f a                 -- hibadobás
--   (<|>) :: f a -> f a -> f a   -- "catch" művelet  (kiejtés: choice)

--  empty az bal és jobb egységeleme (<|>)-nek
--  (<|>) asszociatív

-- Hasonlít a Monoid-ra, viszont specifikusabbak a típusok
-- (<>)   :: a -> a -> a
-- mempty :: a

-- empty <|> fa = fa
-- fa <|> empty = fa
-- (fa1 <|> fa2) <|> fa3 = fa1 <|> (fa2 <|> fa3)

instance Alternative (SM s) where
  empty = SM $ \_ -> Nothing   -- hibadobás
  SM f <|> SM g = SM $ \s -> case f s of
    Nothing -> g s
    x       -> x

-- Parser függvények
type Parser a = SM String a


-- egy parser függvény lényegében: String -> Maybe (a, String)
--                                 input  ->    (kiolvasott érték, maradék String)

-- API parsoláshoz (kombinátoros parsolás)
--  "kombinátoros": bizonyos elemi parser függvények, ezekből összerakhatunk nagyobb parser függvényeket


-- olvassunk egy Char-t az input elejéről, ha igaz rá a függvény
-- ha üres az input, akkor Nothing
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = SM $ \s -> case s of
  []    -> Nothing
  c:str -> if f c then Just (c, str) else Nothing

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' f = do
  str <- get
  case str of
    []    -> empty
    c:str -> put str >> pure c
           -- c <$ put str

-- példa: runSM (satisfy (=='a')) "abbb" == Just ('a',"bbb")
--        runSM (satisfy (=='a')) "bbb" == Nothing

-- konkrét karaktert olvasó parser
char :: Char -> Parser ()
char c = () <$ satisfy (==c)    -- kicseréljük ()-re a végeredményt
                                -- (x <$ fa) = fmap (\_ -> x) fa

-- üres inputot olvasó parser
eof :: Parser ()
eof = SM $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- most már tudunk tetszőleges konkrét karaktersorozatot olvasni
pFoo :: Parser ()
pFoo = char 'F' >> char 'o' >> char 'o'

-- | Egy konkrét String-et olvas
string :: String -> Parser ()
-- string []     = pure ()
-- string (c:cs) = char c >> string cs
string = mapM_ char

-- runSM (string "Foo") "FooFoo" == Just ((),"Foo")

-- választás akárhány konkrét String (vagy Char) között

pFooOrBar :: Parser ()
pFooOrBar = string "Foo" <|> string "Bar"

-- +/* regexek esetén

-- Control.Applicative:
--    some :: Alternative f => f a -> f [a]
--    many :: Alternative f => f a -> f [a]

-- Parser-re specializálva:
--    some :: Parser a -> Parser [a]    -- + regex operátor
--    many :: Parser a -> Parser [a]    -- * regex operátor


-- példa:
-- 0 vagy több 'a' után 0 vagy több 'b' karakter

p1 :: Parser ()
p1 = () <$ (many (char 'a') >> many (char 'b'))

-- 1 vagy több 'a' után 1 vagy több 'b'
p2 :: Parser ()
p2 = () <$ (some (char 'a') >> some (char 'b'))

-- definiáljuk újra a some és many kombinátorokat

-- tudjuk, hogy Alternative f-ből következik Applicative f
-- van pure, (<*>)
-- (>>) Applicative verziója (*>) operátor
-- fa *> fb = (\a b -> b) <$> fa <*> fb

-- kölcsönös rekurzióval
some' :: Alternative f => f a -> f [a]
some' fa = (:) <$> fa <*> many' fa      -- nemüres listát ad vissza

many' :: Alternative f => f a -> f [a]
many' fa = some' fa <|> pure []         -- nemüres listát ad *vagy* üres listát ad


p1' :: Parser ()
p1' = () <$ (many' (char 'a') >> many' (char 'b'))

-- 1 vagy több 'a' után 1 vagy több 'b'
p2' :: Parser ()
p2' = () <$ (some' (char 'a') >> some' (char 'b'))

-- példa Alternative típusra, ami nem parser: Maybe
--    Nothing <|> Just 10 == Just 10
--    Just 10 <|> Just 20 == Just 10


-- ezen a pontot tudunk írni tetszőleges regex parsert
-- valójában: nem csak regex parsert írhatunk
--   nem is csak környezfüggetlen parsert
--   mivel akármilyen rekurzív függvényt írhatunk (Turing-teljes nyelvben vagyunk)
--   (ekvivalens a rekurzív leszállás-al)
--   (parser generátor (LL(k), LR, LALR, stb..))
-- régen: parser generátorok, nyelvtanok gyakorlatban fontosabb volt, mint manapság

-- manapság: Clang, GCC, Rust : ezek mind rekurzív leszálló parser-t használnak
--    ez azért van: sebesség már nem olyan fontos
--                  hibaüzenet elég fontosak
--                  (parser generátor: hibaüzeneteket nehéz jól implementálni)


-- példát: környezetfüggő parser
--    (ha írunk parsert,
--        ami csak Applicative metódust használ: "környezetfüggetlen" nyelv
--                                                ("Parsing Expression Grammar" (PEG))
--        ami Monad metódust is használ:         "környezetfüggő" nyelv


-- példa környezetfüggő parser-re
-- (a^n b^n nyelv)

p3 :: Parser ()
p3 = do
  str <- many (char 'a')      -- 0 vagy több 'a' karaktert olvas
  let len = length str        -- (do-blokkba beszúrahtó let definíció akárhova)
                              -- (ebben az esetben nem kell "in")
  replicateM_ len (char 'b')  -- Control.Monad.replicateM_ : Int-szer végrehajt egy
                              -- monadikus műveletet

-- runSM p3 "aaabbb" == Just ((),"")
-- runSM p3 "aaabb" == Nothing


-- pozitív szám literál felismerése
pPos :: Parser ()
pPos = () <$ some (satisfy isDigit)   -- Data.Char.isDigit


-- egész input pontosan egy darab pozitív szám (olvassuk az input végét is!)
pPos' :: Parser ()
pPos' = pPos >> eof

-- runSM pPos' "32423  fgdofgkko" == Nothing
-- runSM pPos' "32423" == Just ((),"")
