
-- Vizsga, gyak pontok
--------------------------------------------------------------------------------

-- akinek nincs 13 pont, azt a neptun majd ledobja
-- jovő péntekig házi feladatokat BEAD-ni lehet +pontért
--    lehet utána jelentkezni, ha megvan 13 pont

--------------------------------------------------------------------------------

-- Téma:
-- hogyan fordul egy Haskell program?  (lusta funkcionális program)

-- link: https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf

--------------------------------------------------------------------------------

-- Magasabbrendű függvények
--   C: függvény ptr, nincs: closure

-- Python, Javascript: closure gyakori (Java, C#)

-- pl:

f :: Int -> (Int -> Int, Int -> Int)
f x = (\y -> y + x, \y -> y * x)

--               ptr
--                |
--             header | első mező | második mező
--              64bit    64bit         64bit

-- Nem működik: sima függvény ptr!

-- Closure: "capture" lokális változóknak

-- Closure objektum:

-- hozzunk 2 darab *top-level* függvényt:

lam1 :: Int -> Int -> Int
lam1 x y = y + x

lam2 :: Int -> Int -> Int
lam2 x y = y * x

-- closure objektum:
--
--     ptr
--      |
--     függvény ptr | ... |  ... |
--                      N-darab "capture" érték

-- konkrétan:
--     \y -> y + x létrehozzuk
--     &lam1 | x

-- mutable capture:
--    function(y) { x += 10; return (x + y + 10) }    -- mellékhatásos függvényhívás!
-- C++-ban : closure még inkább bonyolódik (byref, copy capture, etc.)

-- Hogyan hívjuk meg a closure-t?

-- Általános eset "generikus hívás" / "ismeretlen hívás"
--

f2 :: Int -> Int -> Int
f2 x y = x * y * y

higherOrder :: (Int -> Int) -> Int
higherOrder f = f 200

  -- Haskell-ben minden függvény curry-zett, és lehet parciálisan applikálni
  --  lassú: - ehelyett codegen próbál N-es C-jellegű függvényhívást létrehozni
  --         - pl: "ismert hívás":  f2 20 30
  --               direkt hívásra fordul (ugyanolyan gyors mint egy C fv hívás)

  -- ismeretlen hívás:
  --   megnézzük, - hogy f-nek hány paraméter kell a futáshoz
  --              - hány paramétert adunk éppen most át a függvénynek
  --       három eset:
  --         1. kevesebb átadott param: (parciális applikáció):
  --             új closure-t létrehozunk, az új paramétereket is elmentjük
  --         2. pont jó számú paraméter: fogjuk a capture-t + az új paramétereket,
  --             és meghívjuk a függvényt
  --         3. "több" paraméter van, mint ami a függvénynek kell
  --             - meghívjuk a függvényt annyi param-al, amennyivel lehet,
  --             - "rekurzívan" újra probáljuk az alkalmazást az eredmény closure-el
  --               és a maradék argumentumokkal


-- példa "túlapplikáció"-ra
id' :: a -> a   -- 1 paraméter
id' x = x

-- 2 paraméteres hívás, ami jól típusozott:
-- id' (\x -> x + 10) 20


-- példa ismeretlen hívásból ismert hívásra konvertálásra:

-- higherOrder (+30) --> (+30) 200 --> 230


-- Lusta kiértékelés
--------------------------------------------------------------------------------

-- lusta példa
lazy :: Int -> (Int, Int)
lazy x = (x + 100, x + 200)

  -- minden lustán kiértékelt kifejezés "thunk"-ra fordul

  -- ptr
  --  |
  -- fv ptr | <lefoglalt szó az eredménynek> | ... | ... |
  --                                            capture

-- első fv ptr
f1 :: Int -> Int
f1 x = x + 100

-- második fv ptr
f2' :: Int -> Int
f2' x = x + 200

-- első thunk:     &f1  | x
-- második thunk:  &f2' | x


-- force-olás: "case" kifejezés a force-olás pontja.
--  (minden mintaillesztésből 1-szeres case illesztés lesz)

-- case lazy n of
--   (x, y) -> case x of
--     x -> x + 4000


-- Mintaillesztés: mindenhol +1 esetet vizsgálunk (thunk-e az adott érték)

-- force-olás során:
--   1. megnézzük, hogy van-e már eredmény
--      ha van: visszaadjuk
--      ha nincs:
--      2. meghívjuk a fv ptr-t a capture-el (gyors hívás, ismert az aritása)
--      3. végeredmény-el felülírjuk a lefoglalt szót a thunk-ban

-- Mi értelme lehet annak, hogy nem cache-eljük az eredményt?
--   pl ha force-olás mellékhatással járhat


-- Strictness elemzés:
--   thunk-okat próbálja kiirtani a generált kódból
--   vélemény: by-default laziness túl nehezen optimalizálható
--             Idris: by default minden strict
--                    (Lazy a) lusta kifejezések típusa

--   GHC feature-ök: {-# language Strict #-}

-- f x = let y = x + 10 in (y, y)    -- strict
-- f ~x = let ~y = x + 10 in (y, y)  -- mégis lusta


-- Core nyelv
--------------------------------------------------------------------------------


--             cukor nélküli Haskell     minimalista lambda calc.


{-
 |  forrásnyelv     full Haskell
 |  Core            cukor nélküli Haskell (pl: nincs IO, nincs typeclass)
 |  STG             gyengén típusozott minimalista lambda kalkulus (pl: f (g x) sincs!)
 |  Cmm             pseudo-assembly
 |  gépi kód vagy LLVM IR

   (LLVM-re fordul: clang, clang++, Swift, Rust, Julia)

-}

{- Core :

- Csak 1-szeres case kifejezés (nincs más mintaillesztés)

   f (Left x) (Right y) | x < 10 = ...
   f (Left x) (Left  _) = ...
   f _        _         = ...

   -- "case tree"
   f x y = case x of
     Left x -> case y of
       Right y -> case x < 10 of
         True -> goto ...
         False -> goto ...
     Right _ -> case ....

  -- "pattern compilation" : minimiális mennyiségű konstruktor tag vizsgálattal
                             megvalósítani

- Nincs typeclass

  class Eq a where
    (==) :: a -> a -> Bool

  instance Eq Bool where
    (==) = eqBool

  f :: Eq a => [a] -> [a] -> Bool
  f xs ys = case xs of
    [] -> case ys of
      [] -> True
      _  -> False
    x:xs -> case ys of
      []   -> False
      y:ys -> case x == y of
        True  -> f xs ys
        False -> False

  - Minden osztályból egy rekord típust csinálok
  - Minden instance-ból rekord értéket csinálok
  - Minden constraint-ből függvény paramétert csinálok

  data Eq a = Eq {(==) :: a -> a -> Bool}

  eqBoolInstance :: Eq Bool
  eqBoolInstance = Eq eqBool

  f :: Eq a -> [a] -> [a] -> Bool
  f eqa xs ys = case xs of
    [] -> case ys of
      [] -> True
      _  -> False
    x:xs -> case ys of
      []   -> False
      y:ys -> case ((==) eqa) x y of
        True  -> f xs ys
        False -> False

  instance Eq a => Eq [a] -->

    eqListInstance :: Eq a -> Eq [a]
    eqListInstance eqa = Eq (f eqa)

  -- (Agda rekordok: egységes szintaxis osztályokkal)

  -- optimalizáció: instance argumentumokat minél inkább inline-olni
  --   constraint-es függvényeket konkrét típusra specializáljuk

  -- Rust trait / C++ template : *mindig minden polimorf* dolgot konkrét típusra
  --   kell specializálni (monomorfizáció)

  --  hátrány: - gyengébb típusrendszer/absztrakció
  --           - megnövekszik a kódméret

  --  előny:   - garancia a sebességre / overhead-re

  --    specializáció / inlining Haskell-ben
  --    {-# SPECIALIZE #-}  {-# INLINE #-}


  -- (Kutatási projekt: magas szintű funkc prog + precíz kontroll fordítás / optimalizálás
  --    fölött (metaprogramozás fontos elem))


--------------------------------------------------------------------------------

- Nincs IO
- Szintaktikus cukorkák nincsenek

-}
