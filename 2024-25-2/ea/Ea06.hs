{-# LANGUAGE DerivingStrategies #-}
-- EA

import Control.Monad.Writer
import Control.Monad.Reader

-- Hogyan is van definiálva a Monád?
{-

-- Mit tudott a Functor?
-- Valamilyen "tároló"
-- Valammenyi elemet "tárol" (0, 1, végtelen, stb)
--                            ^   ^      ^
--                           Maybe Single List
-- (a -> b)-fvel le tudom cserélni az összes elemét
-- anélkül hogy a struktúra megváltozna
-- A tárolónak konstruktorai ugyanott, ugyanolyan sorrendben maradnak
-- Ugyanez igaz a konstruktorok elemire is



class Functor m => Monad m where
  -- v Bindnak
-- fmap :: (a -> b) -> f a -> f b
--      :: f a -> (a ->   b) -> f b
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

-}

-- Pl. A Lista
-- map   :: [a] -> (a ->  b)  -> [b]
{-# LANGUAGE DerivingStrategies #-}
import Control.Monad
bindList :: [a] -> (a -> [b]) -> [b]
---  (a -> [b]) -> [a] -> [b]
-- :t concatMap
bindList [] f = []
--                   [a]    bindList xs f :: [a]
bindList (x : xs) f = f x ++ bindList xs f


{-
[1,2,3,4]
(\x -> replicate x x)

1 -> [1]
2 -> [2,2]
3 -> [3,3,3]
4 -> [4,4,4,4]
   = [1,2,2,3,3,3,4,4,4,4]

-- Nőtt az elemek száma
-- Csökkenhet az elemek száma?

[1,2,3]
(\x -> [])
1 -> []
2 -> []
3 -> []
   = []

Igen, csökkenhet az elemek száma
-}

-- Maybe

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

fmapMaybe :: Maybe a -> (a -> b) -> Maybe b
fmapMaybe Nothing _ = Nothing
fmapMaybe (Just a) f = Just (f a)

-- Monádoknak is vannak törvényei


-- A monádokat fel tudjuk használni
-- Mellékhatásoknak az elkódolására

-- Effektus / Mellékhatás
-- tiszta érték: Int
-- mellékhatásos érték / érték egy effektusban:
-- tisztaérték becsomagolva egy monádba
-- minden monád valamilyen mellékhatást kódol el nekünk típusszinten

-- [a], hogyan különbözik egy a-tól? Nem determinizmus
-- Maybe a, hogyan különözik egy a-tól? Hiba


--    V van egy a-m, ami helyett lehet hogy hibát kaptam
f :: Maybe Int
f = Nothing -- Just 3


--   V ennek a számításnak lehet, hogy több lehetséges eredménye van
g :: [Int]
g = []

newtype Random a = Random { runRandom :: [a] } deriving newtype (Functor, Applicative, Monad)

random :: Int -> Int -> Random Int
random i j = Random [i..j]

getRandomElementFromList :: [a] -> Random a
getRandomElementFromList xs = do
  num <- random 0 (length xs - 1)
  return (xs !! num)

-- IO
-- I/O az egy olyan monád, ami input / output műveletek kódol el
-- getLine :: IO String
-- getLine :: RealWorld -> (RealWorld, String)
-- putStrLn :: String -> IO ()
--                          ^
-- putStrLn :: String -> RealWorld -> (RealWorld, ())

{-
bindIO :: IO a -> (a -> IO b) -> IO b
bindIO :: (RW -> (RW, a))
          -> (a -> RW -> (RW, b))
          -> RW -> (RW, b)

-}

-- Free típus
data FreeRandom a = Between a a | LargerThan a | SmallerThan a


--            a between az mit is csinál?
bindFreeRandom :: FreeRandom a -> (a -> FreeRandom b) -> FreeRandom b
bindFreeRandom = undefined

-- freer-simple
-- polysemy
-- bluefin
-- hestia-effects
-- effectful

-- Monád mint mellékhatás

newtype State s a = State { runState :: s -> (s, a) }
  --deriving newtype (Functor, Applicative, Monad)

-- mi van ha elhagyjuk a bemenetet
-- (s,a) ???
-- mi van ha elhagyjuk a kimenetet
-- s -> a ???

newtype MysteryMonad w a = MysteryMonad { runMysteryMonad :: (w, a) }

-- 1. észrevétel
-- az fmap mint művelet, nem csinál semmit a w típusú paraméterrel
-- w az valamilyen belső struktúra
instance Functor (MysteryMonad w) where
  fmap f (MysteryMonad (w, a)) = MysteryMonad (w, f a)

instance Monoid w => Applicative (MysteryMonad w) where
  pure = return
  (<*>) = ap

instance Monoid w => Monad (MysteryMonad w) where
  -- (>>=) :: MM a -> (a -> MM b) -> MM b
  --          (w,a) -> (a -> (w,b)) -> (w,b)
-- IO probléma:^              ^         ^ ????? melyiket adjam vissza
  -- Valamelyik w-t fel el kell dobni, mert nem tudjuk
  -- mindkettőt felhasználni
  -- combine :: w -> w -> w az jó lenne
  -- (>>=) :: Monoid w => MysteryMonad w a -> (a -> MysteryMonad w b) -> MysteryMonad w b
  (MysteryMonad (w,a)) >>= f = let MysteryMonad (w', b) = f a in
                               MysteryMonad (w <> w', b)
  -- return :: a -> MM a
  -- return :: a -> (w,a)
  --                 ^ ??? honnan szerzünk w-t?
  return a = MysteryMonad (mempty, a)
  -- combine :: w -> w -> w
  -- default :: w
  -- combine művelet jó lenne ha asszociatív
  -- default-nak egységelemének kéne lennie a combine felett
  --
  -- return a >>= f = f a
  -- let (w, b) = f a in (combine default w, b)
  -- = f a
  -- Milyen typeclass teljesítette?
  -- Monoid



-- To sum it up
-- Ha sokat bindolgatunk egymás után
-- akkor lényegében w0 <> w1 <> w2 <> ....
--                 lista ++ lista ++ lista ++ lista ++ ...

-- Ezt a monádot Writer-nek fogják hívni
-- tell :: w -> Writer w ()
-- listen :: Writer w a -> Writer w (w, a)
-- pass   :: Writer w (w -> w) -> Writer w a

verboseAdd :: Int -> Int -> Writer [String] Int
verboseAdd i j = do
  tell ["Az első szám ami itt van az a(z)", show i]
  tell ["A második szám ami itt van az a(z)", show j]
  let z = i + j
  tell ["Na király minden, az eredmény pedig", show z]
  return z


-- s -> (s,a)
-- s -> a

-- Function s a -> (a -> Function s b) -> Function s b
-- (s -> a) -> (a -> s -> b) -> s -> b
bindFunction :: (s  -> a) -> (a -> s -> b) -> s -> b
bindFunction f g s = g (f s) s

-- local :: (s -> s) -> Reader s a -> Reader s a
-- ask :: Reader s s

data SuperSecretFile = MkSSF String

readSSF :: Reader SuperSecretFile (IO String)
readSSF = do
  -- lekérjük a globális konstans értékét
  s <- ask
  let (MkSSF path) = s
  return (readFile path)


-- Monád?
-- Alapból két megközelítése lehet
-- Tároló vagy típus szinten izolált mellékhatás
{-
+--------------------+--------------------+--------------------+
| Tároló             | Effektus           | Mit tudnak?        |
+--------------------+--------------------+--------------------+
| Lista a            | Nem determinizmus  | több darab a       |
+--------------------+--------------------+--------------------+
| Maybe a            | Hiba               | lehet hogy nincs a  |
+--------------------+--------------------+--------------------+
| Tuple w a          | Logging vagy írás  | Loggolni           |
+--------------------+--------------------+--------------------+
| Függvény           | Olvasás            | Globális konstans  |
+--------------------+--------------------+--------------------+
|                    |                    |                    |
+--------------------+--------------------+--------------------+
|                    |                    |                    |
+--------------------+--------------------+--------------------+
-}

-- Következő előadás rövidebb lesz
