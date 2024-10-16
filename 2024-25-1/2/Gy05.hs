{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# HLINT ignore "Use lambda-case" #-}
module Gyak05 where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- Implementáljunk egy beléptető rendszert a metróba!
-- Ezt modellezzük állapotgéppel:
{-

                 ---------    Jegy / Zöld    ----------
             /---|       |------------------>|        |---\
 Tol / Piros |   | Zárva |    Tol / Zöld     | Nyitva |    | Jegy / Sárga
             \-->|       |<------------------|        |<--/
                 ---------                   ----------

-}

-- Az ábrán a kockák a lehetséges állapotokat szimbolizálják
-- A nyilak az átmenetek, például a 'Zárva' állapotból a 'Nyitva' állapotba menő nyíl azt jelneti
--, hogy ha egy Jegyet bele rakunk, zölden világít és a Nyitva állapotra váltunk

-- Definiáljuk az állapotok típusát
data MachineState = Open | Closed
  deriving (Eq, Show)

-- Definiáljuk a fények típusát
data LightColour = R | G | Y
  deriving (Eq, Show)


-- Definiáljuk az átmenetek függvényeit
-- A függvények egy kezdeti állapotból egy végállapotba és egy világító fénybe képeznek
push, insertTicket :: MachineState -> (LightColour, MachineState)
push Open   = (G, Closed)
push Closed = (R, Closed)
insertTicket Open    = (Y, Open)
insertTicket Closed  = (G, Open)

-- Ennek a segítségével például le tudjuk modellezni, hogy Pistike 2x próbál jegy nélkül bemenni
pistike :: MachineState -> ([LightColour], MachineState)
pistike initialState =
  let
    (l1, s1) = push initialState
    (l2, s2) = push s1
    (l3, s3) = insertTicket s2
    (l4, s4) = push s3
  in ([l1, l2, l3, l4], s4)

-- Manuálisan az állapotváltozásokat kezelni viszont, sok boilerplate kóddal jár
-- Igazából azt szeretnénk ha az s1,s2,s3,s4 et nem nekünk kéne manuálisan továbbadni
-- Ebben a formában könnyű elírni (pl : s3 helyett s2, és máris olyan mintha ha egyik sor nem futott volna le)

-- Vegyük észre hogy az "állapotváltozás" folyamatának alakja:
-- s -> (a, s)
-- itt s = MachineState és a = LightColour vagy [LightColour]
-- Az s -> (a,s) alakú függvényeket State Monádnak nevezzük, mivel az ilyen stílusú függvényekre
-- meg lehet a >>= és return műveleteket írni

{-
newtype State s a = State { runState :: s -> (a,s) }

Állapotváltozások kompozíciója
(>>=) ::  State s a    -> (a -> State s b)  -> State s b
háttérben (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)

Nincs állapotváltozás
return :: a -> State s a
háttérben a -> s -> (a,s)

-}

-- Mivel a reprezentáció kicsit komplikáltabb, ezért
-- s -> (a,s) függvényeket a state függvénnyel lehet becsomagolni
pushS, insertTicketS :: State MachineState LightColour
pushS = state push
insertTicketS = state insertTicket

-- Így pistikét kicsit szebben lehet definiálni
pistikeS :: State MachineState [LightColour]
pistikeS = do             --        |
  l1 <- pushS             --        |
  l2 <- pushS             --        |
  l3 <- insertTicketS     --        |
  l4 <- pushS             --        |
  return [l1, l2, l3, l4] -- <------/ A típus diktálja az utolsó kifejezés típusát

-- vagy bindokkal
pistikeS' :: State MachineState [LightColour]
pistikeS' =
  pushS >>= \l1 ->
  pushS >>= \l2 ->
  insertTicketS >>= \l3 ->
  pushS >>= \l4 ->
  return [l1,l2,l3,l4]

-- Nem kell feltétlenül [LightColour]-ba visszatérni
countYellow :: State MachineState Int
countYellow = do
  l1 <- insertTicketS
  l2 <- insertTicketS
  l3 <- insertTicketS
  -- return $ length $ filter (== Yellow) [l1, l2, l3]
  return 0

-- Feladatok
-- Definiáljuk az alábbi átmenetek sorrendjét bindokkal és do-notációval
-- Janika: Jegy, Jegy, Tol
-- Gerike: Tol, Jegy, Tol, Tol
-- Gerike esetén azt adjuk vissza, hányszor 'Piros' volt az átmenetek eredménye

janika :: State MachineState ()
janika = do
  insertTicketS
  insertTicketS
  pushS
  return ()

gerike :: State MachineState Int
gerike = do
  c <- pushS
  c2 <- insertTicketS
  c3 <- pushS
  c4 <- pushS
  return $ length $ filter (== R) [c,c2,c3,c4]


-- Komplikáltabb feladatok
-- Implementáljunk egy 'get' műveletet, amely visszaadja az állapotot
get' :: State s s
get' = state $ \s -> (s , s)
-- Implementáljunk egy 'put' műveletet, amely felülírja az állapotot
put' :: s -> State s ()
put' s = state $ const ((), s)

-- Ezek után nem kell a 'state' függvénnyel szórakozni

-- Példa get/putra: Definiáljuk a safeHead függvényt ami az állapotban lévő lista fejelemét leszedi - ha van neki.
pop :: State [a] (Maybe a)
pop = do {
  s <- get;
  case s of {
    [] -> return Nothing;
    (a : as) -> put' as >> return (Just a);
  };
}

-- Példa get/putra 2: Definiáljuk a take függvényt a belső állapotra (esetleg pop-ot is lehet használni).
takeK :: Int -> State [a] [a]
takeK i | i<= 0 = return []
takeK i = do
  p <- pop
  case p of
    Nothing -> return []
    Just a -> takeK (i -1) >>= \s -> return $ a : s

-- Definiáljuk az alábbi függvényeket!
popLast :: State [a] (Maybe a) -- leszedi az utolsó elemet a listából - ha van.
popLast = get >>= \s -> case s of
  [] -> return Nothing
  [a] -> put [] >> return (Just a)
  (a : xs) -> put xs >> popLast >>= \r -> get >>= \ ys -> put (a : ys) >> return r


sumK :: Num a => Int -> State [a] a -- Kiszedi és összeadja az első K elemet a listából
sumK i | i<= 0 = return 0
sumK i = do
  p <- pop
  case p of
    Nothing -> return 0
    Just a -> do
      k <- sumK (i - 1)
      return $ a + k

labelList :: [a] -> State Int [(a, Int)] -- Minden elemet megcímkéz, a belső állapot a számláló
labelList [] = return [] 
labelList (a : as) = do
  i <- get
  put (i + 1)
  xs <- labelList as
  return ((a , i) : xs)

labelListBW :: [a] -> State Int [(a, Int)] -- Ugyanaz mint az előző csak, hátulról címkéz
labelListBW [] = return []
labelListBW (a : as) = do
  xs <- labelListBW as
  i <- get
  put (i + 1)
  return  $ (a, i) : xs


-- EXCEPT
-- Előző órán hibakezelést a Maybe típussal végeztük, ez viszont különböző hibákat nem tud megkülönböztetni
-- Használjuk az Either típust és egy saját hibatípust!
-- Az alábbi hibák legyenek lehetségesek (mind 0 paraméteres konstruktor)
-- 0-val való osztás hiba
-- Asszertációs hiba
-- Üres lista hiba
data CustomError = DividingByZero | AssertationFault | EmptyList | Overflow | Underflow
  deriving (Eq, Show)

-- Példa: biztonságos osztás
safeDiv :: Integral a => a -> a -> Either CustomError a
safeDiv a 0 = Left $ DividingByZero
safeDiv a b = Right $ a `div` b

-- Az either-t is lehet monadikusan kezelni
bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left e) _ = Left e -- az f sosincs meghívva hiba esetén
bindE (Right a) f = f a

-- Hibák dobása mellet el is kell tudni kapni őket:
catchE :: Either e a -> (e -> Either e a) -> Either e a
catchE (Left e) f = f e
catchE (Right a) _ = Right a

-- Az Eithernek van kicsit általánosabb formája (részletesebben következő órán)
-- Ez az Except Monád
-- newtype Except e a = Except { runExcept :: Either e a }

-- primitív függvények
-- throwError :: e -> Except e a
-- catchError :: Except e a -> (e -> Except e a) -> Except e a

-- Példa throwError/catchErrorra:
-- Dobjunk asszertációs hibát, ha a feltétel nem teljesül
assert :: Bool -> Except CustomError ()
assert False = throwError AssertationFault
assert _ = return ()

-- Biztonságos osztás
safeDivE :: Integral a => a -> a -> Except CustomError a
safeDivE _ 0 = throwError DividingByZero
safeDivE a b = return $ a `div` b

-- Végezzük el a safeDiv műveletet, de ha kivételt dobnánk, adjunk vissza 0-t inkább
safeDivF :: Integral a => a -> a -> Except CustomError a
safeDivF a b = catchError (safeDivF a b) $ const (return 0)

-- Feladatok

-- Hajtogassunk végig az osztás művelettel egy listán, ha 0-val kéne osztani dobjunk kivételt
foldDiv :: Integral a => [a] -> Except CustomError a
foldDiv = foldr (\ a e -> e >>= (\ b -> a `safeDivE ` b)) (return 1)


-- Szimulálni akarjuk a 32 bites számokat, 
-- vagyis tudni szeretnénk ha túlcsordulnánk összeadás/szorzás/kivánosnál
-- akkor egy hibával térnénk vissza
-- Vegyük fel a túlcsordulásra és alulcsordulásra hibákat

u32Max :: Integer
u32Max = 2 ^ 32 - 1

(+++) :: Integer -> Integer -> Except CustomError Integer
(+++) a b = let s = a + b in
  if s > u32Max
    then throwError Overflow {- Valami errort dobjunk itt -}
    else return s

-- Kivonás
(-~-) :: Integer -> Integer -> Except CustomError Integer
(-~-) a b = if b > a 
  then throwError Underflow
  else return $ a - b

-- Szorzás
(***) :: Integer -> Integer -> Except CustomError Integer
(***) a b = let s = a * b in
  if s > u32Max
    then throwError Overflow
    else return s

-- Számoljuk ki két szám távolságának négyzetét:
-- Vagyis végezzük el a 
-- (x2 - x1) ^ 2 + (y2 - y1) ^ 2
-- műveletet

dist :: Integer -> Integer -> Integer -> Integer -> Except CustomError Integer
dist x1 x2 y1 y2 = do
  h <- x2 -~- x1
  j <- y2 -~- y1
  s <- h *** h
  q <- j *** j
  s +++ q
