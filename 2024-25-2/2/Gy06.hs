import Control.Monad.State
import Control.Monad.Reader 
import Control.Monad.Writer 
import Control.Monad.Except
import Control.Monad.IO.Class

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


-- reader :: (r -> a) -> (Reader r a)
bind :: Reader r a -> (a -> Reader r b) -> Reader r b
bind r' f = reader $ \r -> let a = (runReader r') r in runReader (f a) r

-- Primitív műveletek

-- ask :: Reader r r
-- ask = Reader id
-- Visszaadja környezetet, ugyanaz mint a get state-nél

data Env = MkEnv { homeDir :: String, isAdmin :: Bool }

canWriteHere :: String -> Reader Env Bool
-- canWriteHere :: String -> (Env -> Bool)
canWriteHere path = do
  (MkEnv homeDir adm) <- ask
--  False <- canWriteHere
  return (adm || path == homeDir)

-- Írjuk meg az ask-ot

ask' :: Reader r r -- State s s
ask' = reader id

-- local :: (r -> r) -> Reader r a -> Reader r a
-- Lokális megváltoztatja a környezetet a második paraméterben

-- sudo : Kérdezzük le hogy admin-e a felhasználó, 
-- ha igen akkor állítsuk át az Envet : (MkEnv "/root" True)-ra
sudo :: Reader Env () -> Reader Env ()
sudo doas = do
  MkEnv homeDir adm <- ask
  if adm 
    then local (\s -> (MkEnv "/root" True)) doas
    else return ()
-- Írjuk meg a local-t

local' :: (r -> r) -> Reader r a -> Reader r a
local' f ra = reader $ \r -> runReader ra (f r)

-- Feladatok

-- Írjunk egy olyan map függvényt reader segítségével, 
-- amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = runReader (mwiReader f xs) 0

-- Segéd függvény
mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader f []     = return []
mwiReader f (x:xs) = do
  i <- ask
  xs' <- local (\z -> z + 1) (mwiReader f xs)
  return (f i x : xs') 


-- Pl.: (mapWithIndex (\i a -> (i+1) * a) $ take 10 [1,1..]) == [1..10]

data Tree a= Leaf a | Node (Tree a) (Tree a)

-- Számoljuk meg egy fa magasságát Reader-el

heightReader :: Tree a -> Reader Int Int
heightReader (Leaf _) = ask
heightReader (Node left right) = do
  left' <- local (+1) (heightReader left)
  right' <- local (+1) (heightReader right)
  return $ max left' right'

e = Leaf ()
-- runReader (heightReader (Node (Node e (Node e e)) e)) 0 == 3

-- Writer: írási környezet, például loggingra hasznos
-- newtype Writer w a = Writer { runWriter :: (a, w) }
--                                           ^ state-nek elhagytuk a bemeneti paraméterét
-- Primitív műveletek

-- tell :: Monoid w => w -> Writer w () -- üzenet írása, a >>= kombinálja az ezzel írt üzeneteket <>-vel
-- tell w = Writer (w, ())
-- ugyanaz mint a put State-nél
{-
a :: Bool
a = do
  let a = 2
  let b = true
-}
  
{-
let a = c in b
==
(\a -> b) c
-}

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

test :: Writer [String] Int
test = do
  calculation
  calculation
  return 0

calculation2 :: Writer [String] Int
calculation2 = do
  tell ["Na vágjunk bele"]
  res <- silence calculation
  tell ["Elhagyjuk a résszámolás első üzenetét",
   "A többit ismét reportáljuk"]
  --tell (tail messages)
  tell ["Majd eredmény + 1"]
  return (res + 1)


silence :: Monoid w => Writer w a -> Writer w a
silence w = pass $ do
  a <- w
  return (a , ((\m -> mempty) :: Monoid w => w -> w))
{-
   $ do
  a <- w
  return $ (a, mempty)

-}
--                                v alkalmazza a függvényt a saját kiírandó szövegeire
--pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
--pass (WriterT w (a, f)) = WriterT (f w) a

--silence :: Monoid w => Writer w a -> Writer w a
--silence w = pass $ do
--  a <- w
--  return (a, const [])