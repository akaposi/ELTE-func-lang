{-# options_ghc -Wincomplete-patterns #-}

-- ghci, holes, gyakorló feladatok
-- funkc prog jegyzet: http://lambda.inf.elte.hu/ (magasabbrendű függvények, listák)
--    (amit nem fogunk ebből haszálni: listakifejezések, numerikus típusok)

-- feladatmegoldás: aki feladatot csinál, ossza meg a képernyőjét

{-
ghci infok:
parancsok:
  :l <fájl>       betölteni egy fájlt
  :r              újratölt (típusellenőrzés)
  :t <kifejezés>  megadja kifejezés típusát
  :bro            fellistázza a jelenleg betöltött modul összes definícióját
  :browse         --||--
  :i <név>        névhez információt nyomtat (definíció, class, típus, konstruktor)
                  - pl: operátor szintaxisa (precedencia, "fixitás")
                  -     osztály instance-jai, metódusai
-}

{-
Nyelvi/GHC opciók:
  fájl tetjén lehet GHC option-t állítani
  pl: {-# options_ghc -Wincomplete-patterns #-}    -- hiányos mintaillesztések warning
-}


{-
BEAD feladat (10-15 perc)    0, 1, 2 pont jár rá

jövő hét BEAD: magasabbrendű függvény, párok, nem ADT-k (Either, Maybe, lista)
  (akárhányszor fel lehet tölteni megoldást a határidőig)
  (üres megoldás gyakorlati jelenlét)
-}


-- :t fejlett alkalmazása : típus annotáció nélküli definíció
--                          :t-vel kiderítem, hogy mi a típusa

-- map' :: (t -> a) -> [t] -> [a]
map' f []     = []
map' f (x:xs) = f x : map' f xs

infixl 5 ***   -- 5-ös erősségű, balra asszociáló operátor
(***) :: Int -> Int -> Int
(***) x y = 10 * x * y

-- operátor info ghci-ben:

-- :i (***)
-- (***) :: Int -> Int -> Int
-- Defined at ...
-- infixl 5 ***


-- Gyakorló feladatok (ismétlés, függvények, mintaillesztés, ADT-k, osztályok)
--------------------------------------------------------------------------------

-- Definiáld a "xor" műveletet Bool típuson. Használj mintaillesztést,
-- vagy Prelude-ből standard függvényt.
xor :: Bool -> Bool -> Bool
xor = undefined

-- függvények
--------------------------------------------------------------------------------

-- Definiáld a következő függvényeket tetszőlegesen, de
-- típushelyesen és totális függvényként (nem lehet végtelen loop
-- vagy exception).

-- polimorf függvények (meg van szorítva, hogy milyen jól típusos definíciót lehet írni)
--   (*minden* jól típusos totális definíció helyes lesz!)

idInt :: Int -> Int
idInt x = x + 100

idInt2 :: Int -> Int
idInt2 x = x

id' :: a -> a
id' x = x            -- egyetlen lehetséges totális definíció!

id'' :: a -> a
id'' a = a        -- a változó típusa a


f1 :: (a, (b, (c, d))) -> (b, c)
f1 (a, (b, (c, d))) = (b, c)

  -- a, b, c, d *típus*,
  -- a, b, c, d, *változónév/érték*
  -- ugyanazt a nevet adom a változónak, mint ami a típusa

   -- _-t írhatok a programban bármilyen kif. helyére
   -- ghc megmondja a _ helyére milyen típusú kif-t kell írni
   --   hasznos info: - mi a lyuk típusa
   --                 - mi a scope-ban levő nevek/változók típusa

   -- használat: _-t teszünk, finomítjuk
   --     1.  f1 = _
   --     2.  f1 x = _
   --     3.  f1 (a, (b, (c, d))) = _
   --     4.  f1 (a, (b, (c, d))) = (_, _)
   --     5.  f1 (a, (b, (c, d))) = (b, c)

-- data Either a b = Left a | Right b


f2 :: (a -> b) -> a -> b
f2 f a = f a                -- standard: ($)

f2' :: (a -> b) -> a -> b
f2' = ($)                   -- ($) f x = f x

f2'' :: (a -> b) -> a -> b
f2'' = \f a -> f a          -- fv paraméter helyett *mindig* írhatok lambda kifejezést
                            -- nem tudok mintaillesztő paraméterből rögtön lambdát csinálni (de lehet hogy "case" kifejezést + lambdát tudok)

-- példa függvény paraméter (lambda + case)-re alakítására
foo :: Bool -> Int
foo True  = 10
foo False = 20

foo' :: Bool -> Int
foo' = \b -> case b of
  True  -> 10
  False -> 20

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 f g a = f (g a)
   -- type hole használata

   -- _ :: c       --> van-e c-típusú érték scope-ban?
   --              --> ha nincs, van-e olyan függvény, ami c-t ad?
   -- f _          --> _ :: b, van függvény, ami b-t ad
   -- f (g _)      --> _ :: a, van "a" típusú érték
   -- f (g a)

   -- type hole puzzle játék előnye: minimális gondolkodással, ennél jóval bonyolultabb függvényeket
   --   is gyorsan tudunk megadni

   -- MSc-n: nyelvek típusrendszere, típuselmélet, formális szemantika
   --   (ugyanezzel az elvvel, de nem programot írunk, hanem bizonyítást valamilyen rendszerben)

f3' :: (b -> c) -> (a -> b) -> a -> c         -- standard fv. (.)
f3' = (.)

f4 :: (a -> b -> c) -> (b -> a -> c)     -- Haskell-ben -> operátor *jobbra* zárójelez
f4 f b a = f a b
                                       -- standard: flip függvény


-- ugyanaz a típus (zárójel elhagyható!)
f4' :: (a -> b -> c) -> b -> a -> c
f4' = flip
-- a -> b -> c = a -> (b -> c)     (függvényt visszaadó függvény)
-- f :: a -> b -> c
-- x :: a
-- f x :: b -> c      (parciális applikáció)
-- y :: b
-- f x y :: c
-- f x y  ugyanaz a kifejezés mint (f x) y     (függvény alkalmazás *balra* zárójelez)
-- \x y z -> exp  ugyanaz mint  \x -> (\y -> (\z -> exp))
-- (\x y z -> exp)  cukorka az egymásba ágyazott függvényekre
