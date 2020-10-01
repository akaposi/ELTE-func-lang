
-- bead feladat
f :: (a -> Maybe b) -> (a, a, a, a) -> Maybe (b, b, b, b)
f g (a, b, c, d) = do
  a <- g a               -- szándékosan árnyékolok (bind/case: árnyékol)
  b <- g b               -- let/where definíció *nem tud árnyékolni!* (rekurzív definíciók)
  c <- g c               -- let a = g a     :   nem árnyékolja "a"-t, hanem rekurzívan (a = g a)
  d <- g d
  return (a, b, c, d)

-- szép mintaillesztés (illesztés tuple-re)
f' :: (a -> Maybe b) -> (a, a, a, a) -> Maybe (b, b, b, b)
f' g (a, b, c, d) = case (g a, g b, g c, g d) of
  (Just a, Just b, Just c, Just d) -> Just (a, b, c, d)  -- árnyékolás!
  _                                -> Nothing

-- (>>=)  = bind
-- return = Just

-- (harmadik (legszebb) verzió: Applicative)


-- Írjuk meg a következő függvényt úgy, hogy ha a bement tartalmaz "a"-t,
-- akkor a kimenet is tartalmazzon.
-- Definiáljuk a függvényt két féleképpen: csak return-el és (>>=)-al, és csak mintaillesztéssel/Just-al!
f1 :: Maybe (Maybe a) -> Maybe a
f1 (Just ma) = ma
f1 _         = Nothing

-- f1 (Just (Just a)) = Just a
-- f1 _               = Nothing

-- vagy:
-- f1 Nothing         = Nothing
-- f1 (Just Nothing)  = Nothing
-- f1 (Just (Just a)) = Just a

f1' :: Maybe (Maybe a) -> Maybe a
-- f1' mma = mma >>= \ma -> ma

       -- mma :: Maybe (Maybe a)
       -- ma  :: Maybe a

f1' mma = do  -- (do blokk, ami nem return-el ér véget)
  ma <- mma   -- külső Maybe hibázik-e?
  ma

-- előadás: monád törvény: return-nek nincs hatása
--    következik: fenti definíció és az alábbi ugyanaz
-- f1' mma = do  -- (do blokk, ami nem return-el ér véget)
--   ma <- mma   -- külső Maybe hibázik-e?
--   a  <- ma    -- belső Maybe hibázik-e?
--   return a

-- (do nélkül): mma >>= \ma -> ma >>= \a -> return a


-- Alkalmazzuk mindhárom input függvényt az (a, b, c) hármas elemeire. Ha
-- bármelyik függvény Nothing-ot ad, az eredmény legyen Nothing, egyébként
-- Just.
-- Definiáljuk a függvényt két féleképpen: csak return-el és (>>=)-al, és csak mintaillesztéssel/Just-al!
f2 :: (a -> Maybe a') -> (b -> Maybe b') -> (c -> Maybe c')
   -> (a, b, c) -> Maybe (a', b', c')
f2 f g h (a, b, c) = case (f a, g b, h c) of
  (Just a', Just b', Just c') -> Just (a', b', c')
  _                           -> Nothing

f2' :: (a -> Maybe a') -> (b -> Maybe b') -> (c -> Maybe c')
   -> (a, b, c) -> Maybe (a', b', c')
f2' f g h (a, b, c) = do
  a <- f a
  b <- g b
  c <- h c
  return (a, b, c)

-- f2' f g h (a, b, c) =
--   f a >>= \a ->
--   g b >>= \b ->
--   h c >>= \c ->
--   return (a, b, c)

-- f2' f g h (a, b, c) =
--   f a >>= \a -> g b >>= \b -> h c >>= \c -> return (a, b, c)


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.

-- emlékezzünk: mapMaybe

-- Definiáljuk a függvényt két féleképpen: csak return-el és (>>=)-al, és csak mintaillesztéssel/Just-al!
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) = case (f a b, zipWithMaybe f as bs) of
  (Just c, Just cs) -> Just (c:cs)
  _                 -> Nothing
zipWithMaybe f _ _ = Just []

zipWithMaybe' :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe' f (a:as) (b:bs) = do
  c  <- f a b
  cs <- zipWithMaybe' f as bs
  return (c:cs)
zipWithMaybe' f _ _ = return []

-- Általánosítsuk az előző függvényt tetszőleges monádra!
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]   -- (Rust, Swift-ben van típusosztály,
                                                                --  de nincs osztály típusoperátor paraméter!)
zipWithM f (a:as) (b:bs) = do
  c  <- f a b
  cs <- zipWithM f as bs
  return (c:cs)
zipWithM f _ _ = return []


-- State monád
--------------------------------------------------------------------------------
-- (lásd előadás + hs fájl)

-- Monad instance: mellékhatás van egy darab "s" típusú írható/olvasható (mutable) referencia

-- feladat: mutációt adjuk meg csak tiszta függvényekkel
--          hogy a Monad instance implementálja

-- példa: (State Int) ()
-- do n <- get           -- olvasás
--    if n < 0
--      then do
--        put (n + 100)  -- írás
--      else
--        put (n - 100)  -- írás

-- int var = ...
-- if (var < 0) {
--   var += 100
-- } else {
--   var -= 100
-- }

-- Állapot reprezentálása: mindenhol + "s" típusú bemenet
--                                   + "s" típusú kimenet


-- Következő BEAD feladat: valami (magasabbrendű függvény)
newtype State s a = State {runState :: s -> (a, s)}
                  -- kezdeti állapot -> (érték, végső állapot)

-- lekérdezi az állapotot és értékként visszaadja
get :: State s s
get = State (\s -> (s, s))
         -- \jelenlegi állapot -> (érték, kimenő állapot)

-- átírja az állapotot egy konkrét értékre
-- (érték típusa: (), csak a hatás érdekes)
put :: s -> State s ()
put s = State (\_                 -> (()   , s))
            -- \jelenlegi állapot -> (érték, kimenő állapot)

-- tetszőleges (s -> s) függvény-re gondolhatok úgy, mint State műveletre
modify :: (s -> s) -> State s ()
modify f = State (\s              -> (()   , f s))
            -- \jelenlegi állapot -> (érték, kimenő állapot)

--
foo :: State Int Int
foo = State (\n -> if n < 0 then (0, n + 10) else (2, n + 20))
          -- (perspektíva kérdése, (s -> (a, s)) függvényre hogyan gondolunk)
          -- ("s" állapot)
