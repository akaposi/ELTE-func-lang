{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- admin:

-- canvas.elte.hu
--   github  : jelenlegi + korábbi anyagok (jegyzetek, feladatok, mintavizsga)
--   bead    : gyak eleji feladatok beadása + vizsgák beadása
--   teams   : EA (létszám)
--   discord : gyak + konzultáció (párhuzamos screen share)

-- előzetes ismeret:
-- http://lambda.inf.elte.hu/    -- BSc func. prog jegyzet (kezdő Haskell rész)
--  (készség szintű)

-- formátum:
--   EA látogatás nem kötelező, fel van véve
--   gyak: látogatás kötelező (max 3 hiányzás), gyak eleji BEAD feladat számít jelenlétnek
--         (nincsenek felvéve)

-- követelmény:
--   gyakorlat: óra eleji feladat (10-15 perc), mindegyik 0, 1, 2 pont
--              házi feladat (határidő szorgalmi időszak vége) (3 x 4 pont)
--              min 13 pont ebből kell hogy legyen

--   vizsga: 2 óra, feladatmegoldás, gyak követelmény feltétel
--           tárgy jegy: kizárólag a vizsgából adódik


-- Néhány bevezető szó
--------------------------------------------------------------------------------

-- anyag: Haskell ("haladó Haskell")
--        "funkc. prog" : "kezdő Haskell"
--   (lényegében: jelen tárgy "közepes" Haskell)

-- Haskell: funkcionális prog. nyelv
--          "tiszta" funckionális

--          "tiszta": mellékhatások megjelennek a típusokban

--          tiszta függvények előnyei:
--            egyszerű, tesztelni könnyű

--          népszerű nyelvek: (bizonyos) mellékhatások alapból mindig elérhetők,
--                            nem derül ki egy típusból, hogy mi a mellékhatás

--          "tiszta" funkcionális:
--            mellékhatás kiderül a típusból
--              előny: - precízebb típusás
--                     - ha látok egy olyan típust, amin nincs mellékhatás (Int -> Int)
--                       tudom, hogy nincs mellékhatás

--          Haskell: mutáció, exception, IO, async, etb.,
--            "first-class" mellékhatások:
--               saját mellékhatást tudunk definiálni
--                 (nem-determinisztikus programozás)
--                 (logikai programozás, constraint programming)
--              (nem csak szokásos mellékhatások elérhetők, hanem custom "exotikus" mellékhatás is)
--            (Monad osztály)

-- Haskell: típusrendszer, (függő típusos nyelvek: Coq/Agda/Idris/Lean)
--                          MSc-n: Coq/Agda)

-- Haskell: intuitív bevezetés: típuselmélet, logika, kategóriaelmélet, algebra


-- Típusosztályok
--------------------------------------------------------------------------------

-- BSc: Eq, Ord, Show

-- érték benne van-e a listában?
elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

-- motiváció:
--  egyenlőség-vizsgálat :   a -> a -> Bool

eqBool :: Bool -> Bool -> Bool
eqBool True  True  = True
eqBool False False = True
eqBool _     _     = False

eqInt :: Int -> Int -> Bool
eqInt x y = (x == y)

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eqa []     []       = True
eqList eqa (a:as) (a':as') = eqa a a' && eqList eqa as as'
eqList _   _      _        = False

-- végtelen típus: [Bool], [[Bool]], [[[[Int]]]], ((Int, Int), (Int, Int)), stb...

eqListListBool :: [[Bool]] -> [[Bool]] -> Bool
eqListListBool = eqList (eqList eqBool)           -- parciáls applikáció

bigEq :: ([[Int]], [Bool]) -> ([[Int]], [Bool]) -> Bool
bigEq = eqPair (eqList (eqList eqInt)) (eqList eqBool)

-- típusosztály:
--    kódgenerálás, típusok alapján

-- Eq standard név

-- osztály deklaráció, osztály név, osztály paraméter
class Eq' a where
  eq :: a -> a -> Bool       -- osztály metódus (több is lehet, hivatkozhatnak az "a" típusra,
                             --    metódusok típusa tetszőleges (nem feltétlenül függvény)

-- minden Eq függvénynek megfelel egy "instance"

instance Eq' Bool where  -- ha a = Bool, akkor mi az eq definíciója
  eq = eqBool

instance Eq' Int where
  eq = eqInt


    -- constraint a "=>" előtt
instance Eq' a => Eq' [a] where     -- a = [a]
  -- [a] -> [a] -> Bool
  eq []     []       = True
  eq (a:as) (a':as') =      eq a a'          &&             eq as as'
                   -- eq :: a -> a -> Bool          eq :: [a] -> [a] -> Bool
  eq _      _        = False

-- több constraint: zárójelben, vesszővel elválasztva)
instance (Eq' a, Eq' b) => Eq' (a, b) where
  eq = eqPair eq eq   -- (eq "a"-ra, eq "b"-re)


eqListListBool' :: [[Bool]] -> [[Bool]] -> Bool
eqListListBool' = eq    -- típusok alapján visszakapom a korábbi definíciót

bigEq' :: ([[Int]], [Bool]) -> ([[Int]], [Bool]) -> Bool
bigEq' = eq

-- kódgenerálás: "instance rezolúció"
--   más nyelvekben: Rust (trait), Swift (protocol), C++ (régóta próbálják bezetni, "concept" néven)
--   Java/C# protocol: nem ugyanaz!  (tudunk megszorítást csinálni)
--                                   (nem tudunk kódot generálni automatikusan)


-- Algebrai adattípusok (ADT-k)
--------------------------------------------------------------------------------

-- "data" kulcsszó: ADT deklarálása

-- enum típusok:

data Color = Red | Green | Blue
  -- új típus: Color
  -- 3 db új értéket: Red, Green, Blue  (típusuk: Color)

-- mintaillesztés
rotateColor :: Color -> Color
rotateColor Red   = Green
rotateColor Green = Blue
rotateColor Blue  = Red

rotateColor' :: Color -> Color
rotateColor' c = case c of
  Red   -> Green
  Green -> Blue
  Blue  -> Red

-- data Bool = True | False


-- paraméteres típus ("a" paraméter)     (standard típus)
-- data Maybe a = Nothing | Just a
               -- Just: konstruktor egy darab mezővel, aminek a típusa "a"

maybeFun :: Maybe Int -> Maybe Int
maybeFun Nothing  = Just 20
maybeFun (Just x) = Just (x + 20)


-- több mező konstruktorban

data MyData = Con1 | Con2 Int Int | Con3 (Maybe Bool) [Int] [Int]
          -- 0 mező      2 mező            3 mező

-- nyelvi opció bekapcsolása
--  fájl tetejére: {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
f1 :: MyData -> Int
f1 Con1                  = 10
f1 (Con2 x y)            = x
f1 (Con3 (Just b) xs ys) = 20
f1 (Con3 Nothing  xs ys) = 30

f1' :: MyData -> Int
f1' Con1                  = 10
f1' (Con2 x y)            = x
f1' (Con3 (Just b) xs ys) = 20
f1' _                     = error "impossible"
                              -- (nagyon kivételes hiba, amiket nem akarunk kezelni)
                              -- undefined : hibát dob
                              -- error msg : msg üzenetet dobja

-- standard lista típus újradefiniálása: rekurzív ADT
data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- deriving: bizonyos osztály instance-okat automatikusan generálunk egy új data-hoz

-- Derive-olható osztályok:
--  class Eq a where
--    (==) :: a -> a -> Bool

--  class Show a where
--    show :: a -> String

--  class Eq a => Ord a where
--    (<) :: a -> a -> Bool
--    (>) :: a -> a -> Bool
--    (<=) :: ...
--    (>=) :: ...

--  (ghci-beli érték nyomtatás: Show instance-t használ)


l1 :: List Int
l1 = Cons 10 (Cons 20 (Cons 30 Nil))  -- [10, 20, 30]
                                      -- (10 : 20 : 30 : [])
                                      -- (:) 10 ((:) 20 ((:) 30 []))

map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)


------------------------------------------------------------

-- "superclass" megszorítás, megszorítás a class deklaráción:
--   csak akkor írhatunk (Ord' a) instance-t, ha az "a" típusra már van
--   (Eq' a) instance
class Eq' a => Ord' a where
  lt :: a -> a -> Bool
  lte :: a -> a -> Bool

instance Ord' Bool where
  lt False True = True
  lt _     _    = False
  lte = undefined

-- hiba:
-- instance Ord' MyData where

fun :: Ord' a => a -> a -> Bool   -- a superclass metódusokat is mind el tudom érni!
fun x y = eq x y

-- "superclass" / "subclass" : más, mint az OOP fogalmak!

--------------------------------------------------------------------------------
