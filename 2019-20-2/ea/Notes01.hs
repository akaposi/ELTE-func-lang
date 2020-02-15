
-- tárgyi info:
-- github.com/AndrasKovacs/ELTE-func-lang: 2019-20-2 könyvtár

import Prelude hiding (Eq(..), Show(..))


-- Type class
------------------------------------------------------------

-- Probléma: függvény listák egyenlőségének eldöntésére.

-- A következő típus nem elég: [a] -> [a] -> Bool
-- Szükséges extra paraméter az elemek egyenlőségéhez
eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList f []     []     = True
eqList f (x:xs) (y:ys) = f x y && eqList f xs ys
eqList f _      _      = False

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool)
       -> (a, b) -> (a, b) -> Bool
eqPair f g (x, y) (x', y') = f x x' && g y y'

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eqListListBool :: [[Bool]] -> [[Bool]] -> Bool
eqListListBool = eqList (eqList eqBool)

eqListPair :: [(Bool, Bool)] -> [(Bool, Bool)] -> Bool
eqListPair = eqList (eqPair eqBool eqBool)

-- végtelen sok típusra következik mechanikusan az egyenlőség
-- pl. akármilyen mélyen ágyazott Bool-listák ([[[[Bool]]]])

-- Nem szeretnénk minden eq függvényt kézzel kiírni
-- erre megoldás: type class.

class Eq a where          -- class deklaráció
  (==) :: a -> a -> Bool  -- metódus (lehet egy vagy több)
  -- (==) operátorként megadott név

-- Ha a = Bool, akkor mi (==) definíciója
instance Eq Bool where
  -- True  == True  = True
  -- False == False = True
  -- _     == _     = False
  (==) = eqBool   -- elég a korábbi definícióra hivatkozni

-- Ha (a = [a]), akkor (feltéve hogy van Eq a-ra)
-- mi (==) definíciója.
instance Eq a => Eq [a] where
  -- []     == []     = True
  -- (x:xs) == (y:ys) = (x == y) && (xs == ys)
  -- _      == _      = False
  (==) = eqList (==)

instance (Eq a, Eq b) => Eq (a, b) where
  -- (x, y) == (x', y') = (x == x') && (y == y')
  (==) = eqPair (==) (==)


-- Nem szükséges kézzel összerakni a függvényt:
eqListPair' :: [(Bool, Bool)] -> [(Bool, Bool)] -> Bool
eqListPair' = (==)
  -- eqList (eqPair eqBool eqBool)

-- Más nyelvek:
--   Swift protocol, Rust trait : ugyanaz, mint a Haskell type class
--   C#, Java interface: nem ugyanaz
--     különbség: a type class kódot generál a típus struktúrája szerint rekurzívan
--                az interface-nél nincs automatikus kódgenerálás


-- Megjegyzés: a type class-ok felfoghatók egyszerű logikai programokként,
--   amelyek kódot generálnak.

--   logikai programozás (pl: Prolog, Datalog)
--     típusosztályok: egyszerű logikai programok
--     kódgenerálás:   rezolúciós algoritmus (lásd: MSc-s tárgy: logika)

--   Nem Turing-teljes! (ellentétben pl: C++ template-el)


-- Show osztály
------------------------------------------------------------

-- String-ként megjeleníteni
class Show a where
  show :: a -> String

instance Show Bool where
  show True  = "True"
  show False = "False"

instance Show a => Show [a] where
  show []     = "[]"
  show (x:xs) = show x ++ " : " ++ show xs

-- Algebrai adattípusok (részben ismétlés)
------------------------------------------------------------

-- példák:
-- enumerációk: (N-darab különböző értéket tartalmazó típusok)

data Bool' = True' | False'
-- beépített Bool pontosan így van definiálva

-- (típus konstruktor)       (adat konstruktorok)
data   Direction         = South | East | North | West

-- pl: South :: Direction

-- Minteillesztés:
reverseDir :: Direction -> Direction
reverseDir South = North
reverseDir North = South
reverseDir East  = West
reverseDir West  = East


-- újra tudjuk definiálni a lista típust:

-- a : típusparaméter
data List a = Empty | Cons a (List a)  -- rekurzív mező

l1 :: List Bool
l1 = Empty

l2 :: List Bool
l2 = Cons True Empty

l3 :: List Bool
l3 = Cons True (Cons False (Cons True Empty))

-- (egyszeresen láncolt lista)

map' :: (a -> b) -> List a -> List b
map' f Empty       = Empty
map' f (Cons a as) = Cons (f a) (map' f as)

-- korábban:
-- map f []     = []
-- map f (a:as) = (:) (f a) (map f as)


-- Algebrai típusok algebrája
------------------------------------------------------------

-- Miért az a neve, hogy algebrai adattípus?
-- Az új típusokat létrehozó típuskonstruktorok algebrai műveletként
-- értelmezhetők. Megadható 0, 1, (+), (*), (^) típusokkal.

-- Prelude-ben: ()
-- egy elemű enum
data One = One

-- egy elemű típus
one :: One
one = One

-- konstruktor nélküli típus (0 elemű)
data Zero

-- összeadás: a és b lehetséges értékeinek száma összeadódik
data Sum a b = Inj1 a | Inj2 b
-- Prelude: data Either a b = Left a | Right b

x1 :: Sum One One
x1 = Inj1 One

x2 :: Sum One One
x2 = Inj2 One

-- 2 elemű típus
type Two = Sum One One


-- A Zero az összeadás egységeleme:
x3 :: Sum One Zero  -- egy lehetséges elem
x3 = Inj1 One -- egyetlen lehetséges érték

-- 4 elemű típus
type Four = Sum One (Sum One (Sum One One))

-- szorzás: párok típusa
-- egy konstruktor, aminek két mezője van
data Prod a b = Prod a b

-- 4 elemű típus
type Four' = Prod Bool Bool

x4 :: Four'
x4 = Prod True False

-- A One a szorzás egységeleme
type Two' = Prod Bool One


-- Sum kommutativitás:
-- Sum a b értékeinek száma = Sum b a értékeinek száma

-- Jelölés: |a| az "a" típus lehetséges értékeinek száma.

-- |Sum a b| = |Sum b a|
-- |Prod a b| = |Prod b a|

swap :: Prod a b -> Prod b a
swap (Prod a b) = Prod b a

-- Disztributivitás:
-- a * (b + c) = a * b + a * c
-- |Prod a (Sum b c)| = |Sum (Prod a b) (Prod a c)|

-- Extra feladat: minden |a| = |b| egyenlőséghez tartozik egy
-- bijekció "a" és "b" között. Definiáljuk ezt a fenti egyenlethez!
distr1 :: Prod a (Sum b c) -> Sum (Prod a b) (Prod a c)
distr1 = undefined

distr2 :: Sum (Prod a b) (Prod a c) -> Prod a (Sum b c)
distr2 = undefined

-- pl
x5 :: Prod Bool (Sum [Bool] Bool)
x5 = Prod True (Inj1 [False, False])

x6 :: Sum (Prod Bool [Bool]) (Prod Bool Bool)
x6 = Inj1 (Prod True [])

-- x6-ot x5-re átírni: refaktorálás (kiemeljük a közös Bool mezőt)
