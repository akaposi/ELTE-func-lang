{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Gy01 where

{-

-- Tárgy oldala : https://github.com/akaposi/ELTE-func-lang/tree/master/2024-25-2

Tematika:
- Canvas EA oldalán olvasható a hosszab verzió
- Követelmény:
  - Óra elején KisZH (12 * 2 pont) canvasban kb 10-15 perc, csak gyakorlati feladatok
  - Félév folyamán 3 db házi (3 * 4 pont) (első kb félév közepén)
  - Az összes 36 pontból 13-at kell összeszedni, akkor lehet menni vizsgázni
  - A 13 ponton túl a KisZH és a Házik nem kötelezőek
  - Vizsgaidőszakban vizsga az egész féléves tananyagból
  - Előadás (Kaposi Ambrus előadó): Déli Tömb 2-712, Kedd 16:00-17:30
  - Gyakorlatról max 3-szor lehet hiányozni

- A tárgyon tetszőleges IDE és szoftver használható (VSCode, Emacs, Neovim stb) beleértve a Haskell Language Servert
- KisZH folyamán tetszőleges segédezköz használható emberi segítségen kívül (Vizsga TBA)

- Órai file-ok: https://github.com/Akaposi/ELTE-func-lang/tree/master/2024-25-02/2
- GyXX_pre.hs = Óra előtti fájl
- GyXX.hs     = Óra utáni fájl

- A tárgy a Funkcionális Programozás (IP-18FUNPEG) tárgyra épül
- Aki el van maradva: lambda.inf.elte.hu

GHCi emlékeztető:
- :l <fájl>     - betölti a fájlt a GHCi-be
- :r            - újratölti a betöltött fájlokat
- :bro <modul>  - browse rövidítése, kiírja egy modul tartalmát
- :t <kif>      - megmondja egy kifejezés típusát
- :i <azon>     - kiírja egy fv/típus/stb információját (kötési erősség, hol van definiálva stb)
- :set <flag>   - bekapcsol egy flag-et (pl -Wincomplete-patterns)
- :q            - kilépés

Pragmák:
{-# <PRAGMA> <OPCIÓK> #-}
- Ez mindig a fájl tetejére megy
- Fontosabb pragmák:
  - OPTIONS_GHC: bekapcsol GHC flageket, pl -Wincomplete-patterns ami warningot ad ha egy mintaillesztés nem totális
  - LANGUAGE: Nyelvi kiegészítők bekapcsolása, pl InstanceSigs ami engedi az instance-ok függvényeinek az explicit típusozását

-}

-- !
-- Amikor függvényre bementre mintát illesztünk az működés szempontjából megegyezik azzal hogy case elünk rajta
-- Mai téma: Ismétlés (függvények, mintaillesztés, algebrai adattípusok, típusosztályok)
xor :: Bool -> Bool -> Bool
xor True  False  = True 
xor False True   = True
xor b  a   = False

xor' :: Bool -> Bool -> Bool
xor' a b = case (a, b) of
  (True, False) -> True
  (False, False) -> True
  (_ , _) -> False
-- A xor és a xor' ekvivalens (minden bemenetre ugyanazt a kimenetet adja)


-- Több megoldás is lehet (mintaillesztés, beépített függvények)
-- Új "case" kifejezés
{-
case x of
  True -> ...
  False -> ...
-}

-- A kettő ekvivalens
-- Let/Where kifejezések: lokális definíciók:
twelve :: Int
twelve = x + x
  where
    x = 6

twelve' :: Int
twelve' = let x = 6 in x + x

-- !
-- Polimorfizmus: A függvény tetszőleges típusokra működik
id' :: a -> a
id' x = x

-- lehet több típusváltozó is
f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a , (b , (c, d))) = (b , c)

-- Segítség: Hole technológia!
-- Haskellben ha az egyenlőség jobb oldalára _-t írunk, a fordító megmondja milyen típusú kifejezés kell oda

-- Minden függvényre van több megoldás (beépített fügvénnyel pl)

f2 :: (a -> b) -> (a -> b)
f2 f = f  -- 

f2' :: (a -> b) -> a -> b
f2' f a = f a -- ($) beépített függvény applikáció


f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)

f3' :: (b -> c) -> (a -> b) -> (a -> c)
f3' f g = \a -> f (g a) -- == f . g == (.)


f4 :: (a -> b -> c) -> (b -> a -> c)
f4 f b a = f a b -- == flip f


-- Segédfüggvények:
-- fst :: (a,b) -> a
-- snd :: (a,b) -> b

-- Curry
f5 :: ((a, b) -> c) -> (a -> (b -> c)) -- Curryzés miatt a -> b -> c == a -> (b -> c)
f5 f a b = f (a , b)

-- Uncurry
f6 :: (a -> b -> c) -> (a, b) -> c
f6 f (a , b) = f a b

-- Ha az eredménybe függvényt kell megadni használj lambdákat!
-- pl.: \x -> x

f7 :: (a -> (b, c)) -> (a -> b, a -> c)
f7 f = (\a -> fst (f a), \a -> snd (f a))

f8 :: (a -> b, a -> c) -> (a -> (b, c))
f8 (f, g) = (\a -> (f a , g a))

-- ADT-k emlékeztető:
-- Either adattípus. Két konstruktora van, Left és Right, ami vagy a-t vagy b-t tárol:
{-

:i Either
  data Either a b 
    = Left a 
    | Right b

ADT : Algebrai adattípus általánosan

data NEV a b c ...
  = C1 ...
  | C2 ...
  | C3 ...
  | Ci Int Int -- példa

:t Ci
-- Ci :: Int -> Int -> NEV a b c ...

Lehet még :

data NEV a b c ...
  = C1 {f1 :: t, ...}
  | ...
Ilyenkor f1 nek a típusa :

  f1 :: NEV -> t -- Vagyis egy "projekció" vagy pl Javában field accessor
  -- Pseudocode example : 
  class NEV
    field
      f1 :: t
  
  a = new NEV
  a.f1 -- ez nállunk haskellben (f1 a) lesz. Ugye f1 egy függvény

-}

-- Ezeket akkor is megtudom írni ha sima MkPair a b
data Pair a b = MkPair {fst' :: a , snd' :: b}
-- MkPair :: a -> b -> Pair a b
-- fst' :: Pair a b -> a
-- snd' :: Pair a b -> b


f9 :: Either a b -> Either b a
f9 (Left a) = Right a
f9 (Right b) = Left b

f10 :: (Either a b -> c) -> (a -> c, b -> c)
f10 f = (\a -> f (Left a) , seg1)
  where
    seg1 b = f (Right b)
-- f :: Either a b -> c
-- Left :: a -> Either a b
-- Right :: b -> Either a b

-- |----> Either a b -----|
-- |                      |
-- | Left                 | f
-- |                      V
-- a ---------------------> c
--         f . Left

-- f . Left :: a -> c
-- (f . Left , f . Right) ::(a -> c, b -> c) 

f10' :: (Either a b -> c) -> (a -> c, b -> c)
f10' f = (\a -> f (Left a) , \b -> f (Right b))


f11 :: (a -> c, b -> c) -> (Either a b -> c)
f11 (f , g) (Left a) = f a
f11 (f , g) (Right b) = g b

f11' :: (a -> c, b -> c) -> (Either a b -> c)
f11' (f , g) e = case e of
  Left  a -> f a  
  Right b -> g b

-- Bónusz

f12 :: Either (a, b) (a, c) -> (a, Either b c)
f12 (Left  (a , b)) = (a, Left  b)
f12 (Right (a , c)) = (a, Right c)


f13 :: (a, Either b c) -> Either (a, b) (a, c)
f13 (a, Left  b) = (Left  (a , b)) 
f13 (a, Right c) = (Right (a , c))

f14 :: (a -> a -> b) -> ((a -> b) -> a) -> b
f14 f g = -- lépésről lépésre:
  -- elöször a lyuk => _ :: b
  -- b-t csak f tud
  f 
  -- ezután a mindkét lyuk => _ : a
  -- a-t csal g tud
  (g 
    -- Itt a g-nek kell egy (a -> b)
    (\a ->  
          -- Felvesszük a-t és a lyuk => _ : b
          -- b-t még mindig csak f tud
      (f a a)) -- de már van a-nk
      )
  (g (\a -> f a a))

-- Listák emlékeztető
-- Listának két konstruktora van: [] és (:)

-- !
-- Definiáljuk a map függvényt listagenerátorral, rekurzióval és hajtogatással

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (a : xs) = f a : map' f xs

-- Daták és osztályok
-- Ismert típusosztályok: Eq, Ord, Show
-- Ezek definíciója:
{-
class Eq a where
  (==) :: a -> a -> Bool

class Eq a => Ord a where
  (<=) :: a -> a -> Bool

class Show a where
  show :: a -> String
-}

-- Daták

-- Egyszerű data, nincs típusparamétere
-- Legyen két konstruktora, 
-- RGB aminek három szám paramétere van és 
-- HSL aminek szintén három szám paramétere van
data Colour
  = RGB Int Int Int
  | HSL Int Int Int

-- Mik a lista konstruktorai és azok paraméterei?
-- GHCi meg tudja mondani
data List a 
  = Nil -- []
  | Cons a (List a)



-- Bináris fa
-- Ilyen nincs beépítve nekünk kell kitalálni
-- Minden belső csúcsnak pontosan 2 részfája legyen
data Tree a
  = Branch [(Tree a)]
  | Leaf

-- Ezt még átnézzük mégegyszer
-- Írjunk ezekre a datákra instance-okat!
instance Eq Colour where
  (==) :: Colour -> Colour -> Bool -- régebbi GHC-ben nem lehetett ezt kiírni InstanceSigs nélkül
  (RGB a b c) == (RGB a' b' c') = a == a' && b == b' && c == c'
  (HSL a b c) == (HSL a' b' c') = a == a' && b == b' && c == c'
  _ == _ = False

instance Show Colour where
  show :: Colour -> String
  -- unwords " " rak az elemek közé
  -- nem kell sok (++)
  show (RGB a b c) = unwords ["RBG", show a, show b, show c]
  show (HSL a b c) = unwords ["HSL", show a, show b, show c]
  

instance Ord Colour where
  (<=) :: Colour -> Colour -> Bool -- Fura kérdés, színeket nem nagyon lehet így összehasonlítani
  (<=) = undefined

--        Kontextus    Instance head
--         v            v
instance (Show a) => Show (List a) where -- Extra Show a kikötés különben nem tudnák kiírni az elemeket
-- Mivel a kontextusba megkötöttük így az a típusú dolgokra van show
  show :: List a -> String
  show Nil = "[]"
  show (Cons x xs) = show x ++ (show xs)
--                     ^
--                     |- itt használjuk a (Show a) instancet

instance (Eq a) => Eq (List a) where
  (==) :: List a -> List a -> Bool
  (==) = undefined

-- Bónusz
instance (Eq a) => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

data HappyLittleTree a = HappyLittleLeaf a a | HappyLittleNode (HappyLittleTree a) a deriving Show