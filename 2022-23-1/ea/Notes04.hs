
import Control.Monad

-- Monad bevezetés
------------------------------------------------------------

-- osztály: Monad
--    Functor => Applicative => Monad   (4 darab metódus)

-- lásd korábbról: Eq => Ord  alosztály reláció

-- class Functor f
-- class Functor f => Applicative f
-- class Applicative f => Monad f

-- Monad motiváció
------------------------------------------------------------

-- Maybe típust használjuk hibakezelésre
--   Nothing: a hiba reprezentációja

-- ha bárhol Nothing-ot kapunk, legyen a végeredmény Nothing,
-- egyébként Just <map-elt lista>
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) =
  case f a of     -- "hibakód" stílus
    Nothing -> Nothing                -- minden visszatérésen
    Just b  -> case mapMaybe f as of  --   esetszéválasztás kell
      Nothing -> Nothing
      Just bs -> Just (b:bs)

-- hibakód stílus helyett: kivétel dobás/kezelés
-- Maybe Monad instance célja: kivétel stílust lehetővé teszi Maybe-re

-- hibakód kezelést kifaktoráljuk 2 függvénybe

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f  = Nothing
bind (Just a) f = f a

-- írjuk újra mapMaybe-t Maybe mintaillesztés nélkül, bind-al
mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe' f []     = Just []
mapMaybe' f (a:as) =
  bind (f a) $ \b ->
  bind (mapMaybe' f as) $ \bs ->
  Just (b:bs)

  -- explicit zárójelezéssel:
  -- bind (f a) (\b ->
  -- bind (mapMaybe' f as) (\bs ->
  -- Just (b:bs)))

  -- case f a of     -- "hibakód" stílus
  --   Nothing -> Nothing                -- minden visszatérésen
  --   Just b  -> case mapMaybe f as of  --   esetszéválasztás kell
  --     Nothing -> Nothing
  --     Just bs -> Just (b:bs)

  -- imperatív pszeudo-kód:
  -- var b = f(a);
  -- var bs = mapMaybe'(f, as);
  -- return (b:bs);

  -- bind-al imperatív kód stílusát tudjunk Haskell-ben reprodukálni
  --   ";" mint imperatív operátor megfelel a "bind"-nak


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)


mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree f (Leaf a) = case f a of
  Nothing -> Nothing
  Just b  -> Just (Leaf b)
mapMaybeTree f (Node l r) = case mapMaybeTree f l of
  Nothing -> Nothing
  Just l' -> case mapMaybeTree f r of
    Nothing -> Nothing
    Just r' -> Just (Node l' r')

mapMaybeTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree' f (Leaf a)   = fmap Leaf (f a)
mapMaybeTree' f (Node l r) =
  bind (mapMaybeTree' f l) $ \l' ->
  bind (mapMaybeTree' f r) $ \r' ->
  Just (Node l' r')

-- standard:

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b       -- "bind" függvény

-- instance Monad Maybe where
--   return :: a -> Maybe a
--   return a = Just a
--
--   (>>=) = bind

mapMaybe'' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe'' f []     =
  return []
mapMaybe'' f (a:as) =
  f a >>= \b ->
  mapMaybe'' f as >>= \bs ->
  return (b:bs)

-- szintaktikus cukorka (>>=) függvényre: "do notáció"

mapMaybe''' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe''' f []     =
  return []
mapMaybe''' f (a:as) = do
  b  <- f a               -- kiejtése: b-t bind-oljuk
  bs <- mapMaybe''' f as
  return (b:bs)

-- fordítása:

-- do
--   x <- exp1          exp1 >>= \x ->
--   exp2               exp2

-- általánosan Monad instance-okról
-- class Applicative m => Monad m where
--
--   return :: a -> m a
--
--   (>>=)  :: m a -> (a -> m b) -> m b

-- m a : mellékhatásos program, ami "a" típusú értékkel tér vissza
--       hogy mi a mellékhatás, azt "m" specifikálja

-- Maybe a : mellékhatásos program,
--           mellékhatás a hiba lehetősége (Maybe specifikálja)
--           visszatérési érték típusa "a"

-- Monad: custom mellékhatások definiálása

--   return :: a -> m a       olyan művelet, aminek *nincs* mellékhatása

--   (>>=)  :: m a -> (a -> m b) -> m b
--       megfelel a ";"-nek, egymás utáni végrehajtást teszi lehetővé

--    exp1 >>= \x ->         var x = exp1;
--    exp2                   exp2


-- standard definíció (segéd operátor)
--   "konstans bind"
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb

-- szintaktikus cukorka:

-- do
--  exp1             exp1 >>
--  exp2     ==>     exp2 >>
--  exp3             exp3


-- IO monád
------------------------------------------------------------

-- IO :: * -> *
-- instance Monad IO
-- p :: IO a           p egy program, ami input-output műveleteket
--                     hajthat végre mellékhatásként
--                     visszatérési érték típusa "a"

-- (IO primitív típus, nincs "data" definíció hozzá)



-- egy sort olvas a std inputról, visszaadja String-ként
-- getLine :: IO String

-- () olyan típus, aminek egy lehetséges értéke van ("unit")
-- azt értéket úgy írjuk, hogy ()
-- Tuple speciális esete, amikor nincs egy mező sem
--  (0, 1, 2) :: (Int, Int, Int)
--  ()        :: ()

-- kinyomtatja a kapott stringet stdout-ra
--   csak a mellékhatás érdekes, a visszatérési érték nem
--   ilyen esetben ()-ot adunk vissza
-- putStrLn :: String -> IO ()

-- Haskell program: main típusa IO a
p1 :: IO ()
p1 =
  getLine >>= \l ->
  putStrLn l >>
  putStrLn l >>
  putStrLn l

-- ghci-ben, ha IO a típusú kifejezést írunk be, akkor
-- ott az lefut és végrehajtja a mellékhatásokat

p2 :: IO ()
p2 = do
  l <- getLine
  putStrLn l
  putStrLn l
  putStrLn l

p3 :: IO String
p3 = fmap (++"!") getLine     -- IO-beli fmap:
                              --  művelet végeredményére alkalmaz
                              --  függvényt

-- generikus Monad függvények
------------------------------------------------------------

-- korábbi kód
mapMaybe'''' :: Monad m => (a -> m b) -> [a] -> m [b]
mapMaybe'''' f []     =
  return []
mapMaybe'''' f (a:as) = do
  b  <- f a
  bs <- mapMaybe'''' f as
  return (b:bs)

-- standard függvény:
--  mapM :: Monad m => (a -> m b) -> [a] -> m [b]
--

-- példa:

-- mapM (\x -> if x < 0 then Nothing else Just (x + 1)) [0..10]
--   == Just [1,2,3,4,5,6,7,8,9,10,11]

-- mapM (\s -> putStrLn s) ["foo", "bar", "baz"]
--   kinyomtatja a három String-et, visszaad egy [()]-t

-- standard függvény:
--   mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
--    csak végrehajta a műveleteket, ()-ot ad vissza

-- mapM_ putStrLn ["foo", "bar", "baz"]
--  kinyomtatja az összes listaelemet

-- általánosan függvények std elnevezése
--   map    ::            (a ->   b) -> [a] ->   [b]    -- tiszta
--   mapM   :: Monad m => (a -> m b) -> [a] -> m [b]    -- monádikus
--   mapM_  :: Monad m => (a -> m b) -> [a] -> m ()     -- visszatérési
--                                                         érték nélk.

-- import Control.Monad

--  zipWith  ::            (a -> b -> c)   -> [a] -> [b] ->   [c]
--  zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]

-- filter  ::             (a -> Bool)   -> [a] ->   [a]
-- filterM :: Monad m =>  (a -> m Bool) -> [a] -> m [a]

-- példa:
f1 :: [a] -> IO [a]
f1 xs = filterM p xs where
  p :: a -> IO Bool
  p _ = do
    l <- getLine
    case l of
      "ok" -> return True
      _    -> return False

-- beolvas egy sort minden listaelemre, az "ok"-os
-- elemek megtartja a listából
-- f1 [0..5]

-- std imperatív nyelvek
------------------------------------------------------------

-- f : Int -> Int      (default mellékhatások, minden függvény
--                      végrehajthatja őket)

--   bizonyos mellékhatások mindig elérhető
--   Haskell-ben:
--      - alapból nincs mellékhatás
--      - ha szeretnénk, akkor magunknak definiáljuk Monad instance

--      - hátrány: több kódot kell írni, többet kell gondolkodni
---     - előny:
--         - a típusból *tudjuk* hogy tiszta függvény tiszta
--         - do notációt olyan mellékhatásra is tudjuk használni,
--           ami nem is feltétlenül létezik beépítve imperatív nyelvekben
--           "egzotikus" mellékhatások:
--             - nem-determinizmus (pl Prolog/Datalog beágyazás)
--             - STM (software transactional memory)
--             - stb.

--  mit értünk "tiszta" :
--     a mellékhatások megjelennek típusban
--                     "első osztályú" nyelvi feature

--  Haskell-ben még mindig van néhány "implicit" mellékhatás
--    - végtelen loop-ok (nem-terminálás)
--    - memória allokáció
--    - futásidő

-- trade-off a kényelem és a szigorúság között
