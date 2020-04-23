
import Control.Monad

-- Feladat 1: vegyük a következő típust, ami a State és Maybe monádok kombinációja
-- A cél az, hogy az (SM s) monádban elérhetők legyenek a State műveletek (get,
-- put, modify) és a hibadobás lehetősége is.
newtype SM s a = SM {runSM :: s -> Maybe (a, s)}

-- Definiáljuk a Functor instance-t.
instance Functor (SM s) where
  fmap = undefined

instance Applicative (SM s) where
  pure = return
  (<*>) = ap

-- Definiáljuk a következő műveletet, ami hibát dob a Nothing felhasználásával!
-- Legyen a működés a következő: legyen bármilyen (sm :: SM s a) és (s :: s) értékre
-- runSM sm s == Nothing
nothing :: SM s a
nothing = undefined

-- Definiáljuk a catch függvényt, amely lehetőséget ad a futás folytatására
-- hiba esetén.
-- Működés: futassuk az első paraméter műveletet, ha Nothing-al tér vissza, akkor
-- futtassuk a második paramétert.
-- Azaz:  runSM (catch nothing sm) s == runSM sm s     bármely sm, s-re
--        runSM (catch sm1 sm2) s    == runSM sm1 s,   ha  (runSM sm1 s /= Nothing)

catch :: SM s a -> SM s a -> SM s a
catch = undefined

-- definiáljuk a get/put műveleteket!
get :: SM s s
get = undefined

put :: s -> SM s ()
put = undefined

-- Definiáljuk a Monad instance-t!
instance Monad (SM s) where
  return = undefined
  (>>=)  = undefined

--------------------------------------------------------------------------------
