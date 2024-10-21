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
bind r' f = reader $ \r -> let a = ((runReader r') r) in runReader (f a) r

-- Primitív műveletek

-- ask :: Reader r r
-- ask = Reader id
-- Visszaadja környezetet, ugyanaz mint a get state-nél

data Env = MkEnv { homeDir :: String, isAdmin :: Bool }
--MkEnv :: String -> Bool -> Env
--homeDir :: Env -> String
--isAdmin :: Env -> Bool

canWriteHere :: String -> Reader Env Bool
canWriteHere path = do
  MkEnv homeDir adm <- ask
  return (adm || path == homeDir)

-- Írjuk meg az ask-ot

ask' :: Reader r r
ask' = reader $ \r -> r

-- local :: (r -> r) -> Reader r a -> Reader r a
-- Lokális megváltoztatja a környezetet a második paraméterben

sudo :: Reader Env () -> Reader Env ()
sudo doas = do
  MkEnv homeDir adm <- ask
  when adm $ local (\_ -> (MkEnv "/root" True)) doas
-- when :: Bool -> f () -> f ()


sudo' :: Reader Env Bool -> Reader Env Bool
sudo' doas = do
  MkEnv homeDir adm <- ask
  t <- local (\_ -> (MkEnv "/root" True)) doas
  return t 

s :: Reader Env Bool
s = do
  MkEnv homeDir adm <- ask
  return adm 

-- runReader (sudo' s) (MkEnv "" False)


-- Írjuk meg a local-t

local' :: (r -> r) -> Reader r a -> Reader r a
local' f ra = reader $ \r -> runReader ra (f r)

-- Feladatok

-- Írjunk egy olyan map függvényt reader segítségével, amely az egyes listaelemek indexével is összekombinálja az elemeket.
-- Az olvasási környezetben tároljuk a jelenlegi indexet
mapWithIndex :: Integral i => (i -> a -> b) -> [a] -> [b]
mapWithIndex f xs = runReader (mwiReader f xs) 0

mwiReader :: Integral i => (i -> a -> b) -> [a] -> Reader i [b]
mwiReader f [] = return []
mwiReader f (a:as) = do
    i <- ask
    let b = f i a
    bs <- local (+1) (mwiReader f as)
    return $ b : bs

-- Pl.: (mapWithIndex (\i a -> (i+1) * a) $ take 10 [1,1..]) == [1..10]

data Tree a= Leaf a | Node (Tree a) (Tree a)

-- Számoljuk meg egy fa magasságát Reader-el

heightReader :: Tree a -> Reader Int Int
heightReader (Leaf a) = do
    i <- ask
    return i
heightReader (Node l r) = do
    i <- local (+1) (heightReader l)
    i' <- local (+1) (heightReader r)
    return (max i i')

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
  res <- calculation
  tell ["Elhagyjuk a résszámolás első üzenetét", "A többit ismét reportáljuk"]
  -- tell (tail messages)
  tell ["Majd eredmény + 1"]
  return (res + 1)

--                                v alkalmazza a függvényt a saját kiírandó szövegeire
-- pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
-- pass (Writer w (a, f)) = Writer (f w) a

silence :: Monoid w => Writer w a -> Writer w a
silence w = pass $ do
  a <- w
  return $ (a, mempty)

calculation3 :: Writer [String] Int
calculation3 = do
  tell ["Na vágjunk bele"]
  res <- silence calculation
  tell ["Elhagyjuk a résszámolás első üzenetét", "A többit ismét reportáljuk"]
  --tell (tail messages)
  tell ["Majd eredmény + 1"]
  return (res + 1)