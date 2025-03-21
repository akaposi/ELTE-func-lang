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

-- Írjuk meg az ask-ot

ask' :: Reader r r
ask' = undefined

-- local :: (r -> r) -> Reader r a -> Reader r a
-- Lokális megváltoztatja a környezetet a második paraméterben

-- sudo : Kérdezzük le hogy admin-e a felhasználó,
-- ha igen akkor állítsuk át az Envet : (MkEnv "/root" True)-ra
sudo :: Reader Env () -> Reader Env ()
sudo doas = do
  MkEnv homeDir adm <- ask
  if adm
    then local (const (MkEnv "/root" True)) doas
    else return ()
-- Írjuk meg a local-t

local' :: (r -> r) -> Reader r a -> Reader r a
local' f ra = reader $ \r -> runReader ra (f r)

-- Feladatok

-- Írjunk egy olyan map függvényt reader segítségével,
-- amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = undefined

-- Segéd függvény
mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader = undefined

-- Pl.: (mapWithIndex (\i a -> (i+1) * a) $ take 10 [1,1..]) == [1..10]

data Tree a= Leaf a | Node (Tree a) (Tree a)

-- Számoljuk meg egy fa magasságát Reader-el

heightReader :: Tree a -> Reader Int Int
heightReader = undefined

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
