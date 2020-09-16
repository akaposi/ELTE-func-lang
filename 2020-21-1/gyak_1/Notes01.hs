
-- infrastruktúra
------------------------------------------------------------
-- ghci parancsok, ghc, holes

-- ghci parancsok:
--   :l file            -- betöltés
--   :r                 -- újratöltés
--   :bro               -- listázza a jelenleg betöltött definíciókat
--   :t kifejezés       -- kifejezés típusa
--   :i definíciónév    -- "információ" névről  (név: érték definíció, típusnév, osztály, operátor(!))

-- példa: operátor precedenciáját lekérdezni :i-vel:
-- > :i (+)

num :: Int
num = 1000


-- typed holes
--------------------------------------------------------------------------------

-- Ha tetszőleges definícióba _-t írunk, újratöltéskor megkapjuk a _ típusát üzenetben.

f7 :: (a -> b, a -> c) -> (a -> (b, c))
f7 (f, g) a = (f a, g a)
   -- minden lépésnél nézzük meg, hogy mi a lyukak típusa, és aszerint "finomítsuk"
   -- a definíciót! ("tételbizonyítás"-hoz hasonló eljárás)
   -- f7 = _
   -- f7 (f, g) a = _
   -- f7 (f, g) a = (_, _)
   -- f7 (f, g) a = (f _, _)
   -- f7 (f, g) a = (f a, _)
   -- f7 (f, g) a = (f a, g _)
   -- f7 (f, g) a = (f a, g a)

-- feladatok
--------------------------------------------------------------------------------

-- gyakfeladatok_01.hs
