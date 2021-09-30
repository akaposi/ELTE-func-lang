
-- IO monád: getLine, putStrLn, print, read

--  mapM :: Monad m => (a -> m b) -> [a] -> m [b]
--
--  Control.Monad, mapM_, mapM, forever, replicateM, forM_, sequence

--  (>=>), Monad törvények

-- State monad (mutáció mint mellékhatás)
-- Applicative default instance
-- deriving Functor

import Control.Monad

--------------------------------------------------------------------------------

-- IO monád
-- IO :: * -> *
-- IO a     :    I/O mellékhatások + a típusú visszatérési érték
-- IO ()    :    I/O művelet, aminek csak a hatására vagyunk kíváncsiak

-- használat:
--    1. main függvény egy programban: IO () típusú

main :: IO ()
main = putStrLn "Hello world"

--    2. ghci-ben IO a típusú értékek automatikusan lefutnak
--       ghci: implicit IO monádban vagyunk benne

--  getLine  :: IO String             beolvas és visszad egy sort
--  putStrLn :: String -> IO ()       kinyomtat egy String-et (+új sort)
--  print    :: Show a => a -> IO ()  kinyomtat egy Show instance-os értéket

--       ghci-ben minden kifejezés amit beütünk, "print"-el nyomtatódik vissza

--  minél több programrész *nincs* IO-ban
--      - minél több tiszta függvény       (refaktorálás + tesztelés szempontjából)
--      - interakciós réteg: IO-ba tesszük

-- "Effect system" : mellékhatások kezelése / strukturálása
--                   Monad : csak egy lehetséges megoldás

-- Int beolvasása:
-- read :: Read a => String -> a
-- read :: String -> Int,          kivételt dob, ha nem lehet olvasni

p1 :: IO ()
p1 = do
  l <- getLine

  let n :: Int       -- do blokk belsejében lehet "let"-et "in" nélkül írni
      n = read l

  -- let x = "foo"

  print [0..n]

-- do notáció nélkül
p1' :: IO ()
p1' =
  getLine >>= \l -> let n = read l :: Int in print [0..n]

-- egymás után való végrehajtás (második művelet nem függ az első visszatérési értékétől)
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb

p2 :: IO ()
p2 = do
  print "hello"
  print "hello"
  print "hello"

p2' :: IO ()
p2' =
  print "hello" >> print "hello" >> print "hello"


-- alosztályozás:
-- Functor m => Applicative m => Monad m

-- getLine :: IO String
-- read    :: String -> Int
-- fmap    :: (a -> b) -> f a -> f b

-- fmap read getLine :: IO Int
-- IO esetén "fmap f ma" végrehajtja ma-t, és "f"-et alkalmazza a végeredményen

-- Control.Monad-ból:

-- replicateM :: Monad m => Int -> m a -> m [a]
--      Int-szer végrehajtja az "m a" műveletet, és listában visszaadja az eredményeket

-- replicateM_ :: Monad m => Int -> m a -> m ()

--   monádikus függvények: név végén underscore: eldobja a visszatérési értéket

-- annyiszor nyomtatom vissza a sort, mint a beolvasott szám
p3 :: IO ()
p3 = do
  n <- fmap read getLine
  l <- getLine
  replicateM_ n (putStrLn l)

-- hasonló: mapM függvény
replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n ma
  | n <= 0    = return []
  | otherwise = do
      a  <- ma
      as <- replicateM' (n - 1) ma
      return (a:as)

replicateM_' :: Monad m => Int -> m a -> m ()
replicateM_' n ma
  | n <= 0    = return ()
  | otherwise = ma >> replicateM_' (n - 1) ma

-- replicateM_' :: Monad m => Int -> m a -> m ()
-- replicateM_' n ma
--   | n <= 0    = return ()
--   | otherwise = do
--       ma
--       replicateM_' (n - 1) ma

-- Maybe esetén mit csinál a replicateM?
--   nem túl értelmes, replicateM n Nothing  == Nothing
--                     replicateM n (Just a) == Just (replicate n a)


-- Control.Monad-ból
-- forever :: Monad m => m a -> m b       loop-ban ismétli a műveletet
--                                        mivel soha nem tér vissza,
--                                          tetszőleges típussal nem tér vissza

loop :: a      -- minimalista rekurzív végtelen loop
loop = loop

forever' :: Monad m => m a -> m b
forever' ma = ma >> forever' ma

p4 :: IO ()                -- Ctrl+c+c : ghci-ben interrupt
p4 = forever $ do
  l <- getLine
  putStrLn (l ++ ", " ++ l)

------------------------------------------------------------

-- mellékhatásos függvények kompozíciója
-- (.)      ::            (b ->   c) -> (a ->   b) -> (a ->   c)
-- flip (.) ::            (a ->   b) -> (b ->   c) -> (a ->   c)
-- (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)

f1 :: Int -> Maybe Int
f1 x = if x > 30 then Nothing else Just (x + 10)

f2 :: Int -> Maybe Int
f2 = f1 >=> f1 >=> f1     -- f1 . f1 . f1 (monadikus formában)

------------------------------------------------------------

f3 :: Int -> IO [String]
f3 n = replicateM n getLine      -- n-szer beolvas egy sort

-- class Functor m => Monad m where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- (f3 >=> return) n
--   ==
-- do ss <- f3 n
--    return ss
--   ==
-- f3 n


-- törvények:
--   (return mellékhatás-mentes)
--    1.      return >=> f = f
--    2.      f >=> return = f

--   (asszociativitás)
--    3.      (f >=> g) >=> h = f >=> (g >=> h)

-- 3. törvény bind-al.

-- do a <- ma
--    b <- mb a
--    mc b
--
--  ==
--
-- do b <- do a <- ma
--            mb a
--    mc b

-- lényeg: egy monádban csak a műveletek sorrendje számít, a csoportosítása nem
--         mi lenne, ha számítana a csoportosítás? (nem utasítások listája, hanem "fája")

-- Monad osztály + törvények :
--    minimális specifikáció arra, hogy mi egy értelmes (jól viselkedő) imperatív/mellékhatásos program
--    1. van return (hatás nélküli művelet)
--    2. a program műveletek listája, csak a sorrend számít


-- State monád
--------------------------------------------------------------------------------

-- mellékhatás: pontosan 1 darab írható-olvasható (mutábilis)
--   - custom hatásként a mutációt implementáljuk
--   - háttérben: immutábilis
--     API: mutációt nyújt

--   (viszont Haskell nyelvben "igazi" primitív mutáció + mutábilis adatstruktúrák is elérhetők
--    nem részei az anyagnak)

-- mező notáció:
data Foo = Foo {first :: Int, second :: Int}
  deriving Show

-- first :: Foo -> Int
-- second :: Foo -> Int

newtype State s a = State {runState :: s -> (a, s)}

-- State s a      :     mellékhatásos művelet, ami 1 db "s" típusú változót írhat/olvashat
--                      és "a" típusú értéket ad vissza

--  bemenő állapot    (visszatérési érték, új állapot)
--    s     ->        (a, s)

--  f1, f2, f3 :: s -> (a, s)       végig kell vinni az "s" állapotot a függvényhívásokon

-- példa:
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- írjunk egy függvényt, bejárás sorrendjében 0-tól beszámozza a leveleket

-- pl: label (Node (Node (Leaf True) (Leaf True)) (Leaf False))
--      == Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (False, 2))

label :: Tree a -> Tree (a, Int)
label t = fst (go t 0) where
  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a) n = (Leaf (a, n), n + 1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')

-- -- imperatív pszeudokód
-- label :: Tree a -> Tree (a, Int)
-- label t = do
--   var n = 0;              -- mutábilis változó

--   let go (Leaf a)   = return Leaf (a, n++)
--       go (Node l r) = return Node(go l, go r)

--   return (go t)

-- State monáddal az imperatív stílusban lehet definiálni
