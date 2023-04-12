
{-
Haskell-ben láttunk: tiszta függvényeket

- 1. Mindenképp kell mellékhatás (példa: IO)
- 2. Bizonyos programok szebbek és hatékonyabbak mellékhatással
     - C nyelv: hibakód (hasonlít: Nothing/Just)
       - ehelyett: kivétel dobás + elkapás
          - (implicit propagáció, csak dobás/elkapás helyén kell
             fogalkoznunk hibákkal)
     - Algoritmusok: mutáció, gyakran tömbök írása, hatékonysághoz kell

Haskell-ben:
  - Default: nincs mellékhatás
    előny?  ha (f :: Int -> Int), akkor típusból kiderül, hogy ez tiszta
      - jól tesztelhető, refaktorálható, stb.
  - A programozó custom mellékhatást tud használni + definiálni
    Megjelenik a típusban a hatás.
  - Monad osztály: minden instance egy mellékhatást definiál
    - néhány primitív (builtin) instance
    - sok user által megadható instance
    - más nyelvekben ami van, az mind elérhető:
        exception, mutáció, IO, konkurrencia, async
    - de lehet "exotikus" mellékhatásokat is definiálni:
         - nem-determinizmus, (általánosítva: logikai programozás)
         - anyag része: parser Monad
         - Tardis (utána lehet nézni)
               (time-traveling State monád)
         - STM (software transactional memory)
               (atomikus műveletek)

-}

-- Maybe-re gondolok úgy, mint hibalehetőség
-- Nothing: hiba
-- ne "case"-el programozzak Maybe a értékekkel, hanem throw/catch
-- Either e a    (Left e : kivétel, amiben "e" adat van)
--               (Right a : sikeres eredmény)


data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Eq, Show)

-- map-elő függvény hibázhat

-- Ha (a -> Maybe b) függvény bárhol Nothing-ot ad, akkor
-- az egész végeredmény legyen Nothing. Egyébként legyen
-- a végeredmény Just-ban a map-elt fa.
mapTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree f (Leaf a) = case f a of
  Nothing -> Nothing
  Just b  -> Just (Leaf b)
mapTree f (Node t1 t2 t3) = case mapTree f t1 of
  Nothing -> Nothing
  Just t1 -> case mapTree f t2 of
    Nothing -> Nothing
    Just t2 -> case mapTree f t3 of
      Nothing -> Nothing
      Just t3 -> Just (Node t1 t2 t3)

{-
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b      -- "bind"

instance Monad Maybe where
  return :: a -> Maybe a
  return = Just

  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  (>>=) ma f = case ma of
    Nothing -> Nothing
    Just a  -> f a
-}

-- Írjuk meg a függvényt, return és (>>=) használatával,
-- "case" és "Nothing/Just" nélkül
mapTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree' f (Leaf a) =
  f a >>= \b ->
  return (Leaf b)           -- (ma >>= return . f) == fmap f ma
mapTree' f (Node t1 t2 t3) =
  mapTree' f t1 >>= \t1 ->
  mapTree' f t2 >>= \t2 ->
  mapTree' f t3 >>= \t3 ->
  return (Node t1 t2 t3)

-- imperatív pszeudokód:
-- mapTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
-- mapTree' f (Leaf a) = {
--   var b = f(a);
--   return (Leaf b);
-- mapTree' f (Node t1 t2 t3) =
--   var t1 = mapTree' f t1;
--   var t2 = mapTree' f t2;
--   var t3 = mapTree' f t3;
--   return (Node t1 t2 t3);

-- mapTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
-- mapTree' f (Leaf a) = {
--   return Leaf(f(a));
-- mapTree' f (Node t1 t2 t3) =
--   return Node(mapTree' f t1, mapTree' f t2, mapTree' f t3);

mapTree'' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree'' f (Leaf a) = do
  b <- f a
  return (Leaf b)           -- (ma >>= return . f) == fmap f ma
mapTree'' f (Node t1 t2 t3) = do {
  t1 <- mapTree'' f t1;
  t2 <- mapTree'' f t2;
  t3 <- mapTree'' f t3;
  return (Node t1 t2 t3);
  }

  -- let x = 10; y = 20 in ..
  -- case x of Nothing -> 10; Just _ -> 20

-- "do notáció"

--   explicit               do notáció
-- ma >>= \x ->          do x <- ma            "bind" x
-- mb                       mb

-- ma >>= \_ ->          do ma
-- mb                       mb

------------------------------------------------------------
-- IO monád: beépített, user által *nem* definiálható

-- putStrLn :: String -> IO ()        (kinyomtat egy String-et)
-- getLine  :: IO String
-- print    :: Show a => a -> IO ()   (kinyomtat egy Show-olható értéket)

foo :: IO ()
foo = do
  line <- getLine         -- var line = getLine();
  putStrLn line           -- putStrLn(line);
  putStrLn line           -- putStrLn(line);

-- speciális ghci viselkedés: ha IO a típusú értéket ütünk be, akkor

-- main :: IO ()
-- main = ...

------------------------------------------------------------
-- class Monad

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f [] = return []
-- mapM f (a:as) = do
--   b <- f a
--   bs <- mapM f as
--   return (b:bs)

-- általános konvenció:
--    map   ::            (a ->   b) -> [a] ->   [b]
--    mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
--    mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

--    forM  = flip mapM
--    forM_ = flip mapM_

--  filterp
--  filterM
--  filterM_

-- zipWith
-- zipWithM
-- zipWithM_

-- Control.Monad-ban van sok ilyen függvény

--------------------------------------------------------------------------------

-- newtype IO a = IO (State# RealWorld# -> (# a, State# RealWorld# #) )

-- unsafePerformIO :: IO a -> a

-- foo :: Int -> Int
-- foo x = unsafePerformIO (do {print "malac"; return (x + x)})


-- IO művelet reprezentációja:
--    függvény: bemenetként az "világ" állapotát megkapja,
--              tuple-ben kimenetként a "világ" új állapotát visszadja
