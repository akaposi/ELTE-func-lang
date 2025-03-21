import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.IO.Class


-- STATE    : s -> (s,a)
-- WRITER   : (s,a)
-- READER   : s -> a
-- IDENTITY : a

-- A State monád állapot változást reprezentált
-- Vegyünk két új Monádot:


-- Reader: olvasási környezet, például globális konstans
-- newtype Reader r a = Reader { runReader :: r -> a }
--                                                ^ state-nek elhagytuk a kimeneti paraméterét

-- Hogy néz ki a bind Reader-er

{-

    Reader r a = r -> a
    és a Reader r a egy monád

    bind :: m a -> (a -> m b) -> m b
    bind :: Reader r a -> (a -> Reader r b) -> Reader r b
    bind :: (r -> a)   -> (a ->  (r -> b) ) -> (r -> b)
-}

bind :: Reader r a -> (a -> Reader r b) -> Reader r b
bind r' f = undefined

-- Primitív műveletek

-- ask :: Reader r r
-- ask = Reader id
-- Visszaadja környezetet, ugyanaz mint a get state-nél

data Env = MkEnv { homeDir :: String, isAdmin :: Bool }

canWriteHere :: String -> Reader Env Bool
canWriteHere path = do
  MkEnv homeDir adm <- ask
  return (adm || path == homeDir)


isAdminR :: Reader Env Bool -- megvizsgálja, hogy a felhasználó admin-e
isAdminR = do
  MkEnv homeDir isAdmin <- ask
  return isAdmin

homeDirR :: Reader Env String -- visszaadja a felhasználó home directoryját
homeDirR = do
  MkEnv homeDir _ <- ask
  return homeDir

areWePistike :: Reader Env Bool -- megnezni, hogy a home directory egyenlo-e "/home/pistike"-vel
areWePistike = do
  MkEnv homeDir _  <- ask
  return (homeDir == "/home/pistike")




-- local :: (r -> r) -> Reader r a -> Reader r a
-- Lokális megváltoztatja a környezetet a második paraméterben

runInHomePistike :: Reader Env a -> Reader Env a
runInHomePistike r = do
  a <- local (\(MkEnv _ admin ) -> MkEnv "/home/pistike" admin) r
  return a

-- sudo : Kérdezzük le hogy admin-e a felhasználó,
-- ha igen akkor állítsuk át az Envet : (MkEnv "/root" True)-ra
sudo :: Reader Env () -> Reader Env ()
sudo doas = do
  MkEnv homeDir adm <- ask
  if adm
    then local (const (MkEnv "/root" True)) doas
    else return ()
-- Írjuk meg a local-t


-- Feladatok

-- Írjunk egy olyan map függvényt reader segítségével,
-- amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = runReader (mwiReader f xs) 0

-- Segéd függvény
mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader f [] = return []
mwiReader f (x : xs) = do
  i <- ask
  xs' <- local (+1) $ mwiReader f xs
  return (f i x : xs')

-- Pl.: (mapWithIndex (\i a -> (i+1) * a) $ take 10 [1,1..]) == [1..10]

data Tree a= Leaf a | Node (Tree a) (Tree a)

-- Számoljuk meg egy fa magasságát Reader-el

{-

          a
       /    \
i= 1  [b]      e
     /  \
    c    [d]


-}


--                               v a mélység
heightReader :: Tree a -> Reader Int Int
heightReader (Leaf _ ) = ask >>= \i -> return (i + 1)
heightReader (Node l r) = do
  l' <- local (+1) $ heightReader l
  r' <- local (+1) $ heightReader r
  return (max l' r')


--                         V akkumulalni az eredményt
reverseR :: [a] -> Reader [a] [a]
reverseR [] = ask
reverseR (x : xs) = local (x:) $ reverseR xs

-- e = Leaf ()
-- runReader (heightReader (Node (Node e (Node e e)) e)) 0 == 3

-- Writer: írási környezet, például loggingra hasznos
-- newtype Writer w a = Writer { runWriter :: (a, w) }
--                                           ^ state-nek elhagytuk a bemeneti paraméterét
-- Primitív műveletek

-- tell :: Monoid w => w -> Writer w () -- üzenet írása, a >>= kombinálja az ezzel írt üzeneteket <>-vel
-- tell w = Writer (w, ())
-- ugyanaz mint a put State-nél

calculation :: Writer [String] Int
calculation = do
  tell ["1-esel kezdünk"]
  let x = 1
  tell ["Aztán egy 2-es"]
  let y = 2
  tell ["Majd az összeg", "Egyszerre többet is tud loggolni"]
  return (x + y)

--                         v lefuttatja ezt a writert
-- listen :: Monoid w => Writer w a -> Writer w (a, w) -- és visszaadja a loggolásait
-- listen (Writer res) = Writer (res, res)

calculation2 :: Writer [String] Int
calculation2 = do
  tell ["Na vágjunk bele"]
  (res, messages) <- listen calculation
  tell ["Elhagyjuk a résszámolás első üzenetét", "A többit ismét reportáljuk"]
  tell (tail messages)
  tell ["Majd eredmény + 1"]
  return (res + 1)

--                                v alkalmazza a függvényt a saját kiírandó szövegeire
-- pass :: Monoid w => Writer w (a, w -> w) -> Writer a
-- pass (Writer w (a, f)) = Writer (f w) a

--silence :: Monoid w => Writer w a -> Writer w a
--silence w = pass $ do
--  a <- w
--  return (a, const [])
copyToWriter :: [a] -> Writer [a] () -- minden elemet kiir az irasi kornyezetbe
copyToWriter = tell

copyToWriterNTimes :: [(a, Int)] -> Writer [a] () -- minden elemet kiir az irasi kornyezetbe annyiszor amennyi a mellete levo szam
copyToWriterNTimes [] = return []
copyToWriterNTimes ((x, i) : xs) = tell (replicate i x) >> copyToWriterNTimes xs
-- tell (concatMap (flip uncurry replicate) xs)
-- copyToWriterNTimes ((x, 0) : xs) = copyToWriterNTimes xs
-- copyToWriterNTimes ((x, i) : xs) = do
--   tell [x]
--   copyToWriterNTimes ((x, i - 1) : xs)


noduplicates :: Eq a => Writer [a] () -> Writer [a] [a]
noduplicates w = do
  (_, r) <- listen w
  return r
-- visszaadja az elso parameterben kiirt uzeneteket
