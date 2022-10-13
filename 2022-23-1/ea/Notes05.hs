{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

-- Monád törvények
-- Generikus függvények folyt, replicateM, forever
-- State monád
--------------------------------------------------------------------------------

import Control.Monad hiding (replicateM, replicateM_, forever)


-- Törvények:

-- class Functor f where ..
-- class Functor f => Applicative f where ...

-- class Applicative m => Monad m where
--   return :: a -> m a                       -- rögtön "visszadjuk" az "a" értéket, mellékhatás nélkül
--   (>>=)  :: m a -> (a -> m b) -> m b       -- két műveletet egymás után elvégez, ahol a második művelet függhet az első
--                                            -- visszatérési értékétől

-- var x = f(y);           f y >>= \x ->
-- prog                    prog

-- p :: m a     p egy mellékhatásos program, "a" a visszatérési érték típusa,
--              "m", "Monad m" instance megadja a mellékhatást

-- pl, Monad Maybe : mellékhatás hiba dobása (mint kivétel)
--     Monad IO    : mellékhatás tetszőleges input/output művelet

-- importálható Control.Monad-ból
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a = do
  b <- f a
  g b

-- (.)      ::            (b -> c  ) -> (a -> b  ) -> (a -> c  )
-- flip (.) ::            (a -> b  ) -> (b -> c  ) -> (a -> c  )
-- (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)     "monádikus kompozíció"

-- monád törvények:

-- 1. törvény: return egységeleme a >=>-nak (nem lehet return-nek mellékhatása)
--       f >=> return = f
--       return >=> f = f

-- átírva do-notációra:
--      do a <- ma          =    ma
--         return a

--     do a <- return a     =    f a
--        f a

-- 2. törvény: (>=>) asszociatív ("szekvencialitás": csak a műveletek sorrendje számít, a csoportosítása nem)
--
--    (f >=> g) >=> h = f >=> (g >=> h)

-- opcionális házi: Maybe-re igazak-e a törvények?


--------------------------------------------------------------------------------

-- replicate  :: Int -> a -> [a]

-- n-szer végrehajtjuk a műveletet, és visszaadjuk az összes értéket egy listában
-- replicateM   :: Monad m => Int -> m a -> m [a]
-- replicateM_  :: Monad m => Int -> m a -> m ()

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma | n <= 0 = return []
replicateM n ma = do
  a  <- ma
  as <- replicateM (n - 1) ma
  return (a:as)

  -- do notáció nélkül
  -- ma >>= \a ->
  -- replicateM (n - 1) ma >>= \as ->
  -- return (a:as)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n ma | n <= 0 = return ()
replicateM_ n ma          = ma >> replicateM_ (n - 1) ma

-- végtelenül ismételjül az adott műveletet
forever :: Monad m => m a -> m b   -- az olyan művelet, ami soha nem tér vissza értékkel,
forever ma = ma >> forever ma      -- annak a visszatérési érték típus tetszőleges

-- Ctr-c-c az interrupt ghci-ben:
p1 :: IO ()
p1 = forever $ do
  l <- getLine
  putStrLn l
  putStrLn l

loop :: a      -- loop mint rekurzív definíció, tetszőleges típusú (mert soha nem ad értéket)
loop = loop


-- State monad
--------------------------------------------------------------------------------

{- motiváció:

- Írható-olvasható mutábilis referenciákat szeretnénk használni
- (háttérben minden immutábilis, csak egy interfészt kapok mutációra)
- (ha tényleg mutációt akarok használni Haskell-ben: IO, ST, STM monádokban lehet valódi mutációt csinálni)

-}

-- Motiváló példa: mutáció szebb, mint az immutábilis megoldás
-- Fa bejárás, bejárás közben nyilván tartunk valamilyen adatot

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

-- balról-jobbra bejárási sorrendben beszámozzuk (0-val kezdve) a leveleket.

-- pl:
-- label (Node (Leaf ()) (Leaf ())) == Node (Leaf ((), 0)) (Leaf ((), 1))
-- label (Node (Leaf ()) (Node (Leaf ()) (Leaf ()))) == Node (Leaf ((), 0)) (Node (Leaf ((), 1))
--                                                                                (Leaf ((), 2)))

label :: Tree a -> Tree (a, Int)
label t = fst (go t 0) where

    -- Int számolja, hogy hány Leaf-et láttam eddig
  go :: Tree a -> Int -> (Tree (a, Int), Int)      -- "n" változó helyett egy mutable referenciát
  go (Leaf a) n = (Leaf (a, n), n + 1)             -- szeretnénk használni
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')

  -- ismételt case-et akarjuk implicit módon egy Monad instance-al megoldani

-- példa rekord mező jelölésre:
--   data Foo = Foo {foo1 :: Int, foo2 :: String, foo3 :: [Bool]}
--   foo1 :: Foo -> Int
--   foo2 :: Foo -> String
--   foo3 :: Foo -> [Bool]

-- rekord 1 db mezővel
newtype State s a = State {runState :: s -> (a, s)}

-- State    :: (s -> (a, s)) -> State s a        (wrapper)
-- runState :: State s a -> (s -> (a, s))        (unwrapper)

--   bemenő állapot     (visszatérési érték, kimenő állapot)
--        s              -> (a, s)

-- prog :: State s a
--   művelet, ami egy "s" típusú értéket (állapotot) módosít, "a" típusú értéket ad vissza

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> case g s of (a, s) -> (f a, s)


instance Applicative (State s) where -- töltelék kód, minden esetben ugyanez lesz
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a    -- nem módosítjuk az állapotot
  return a = State $ \s -> (a, s)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s
       -- g                 :: a -> State s b
       -- g a               :: State s b
       -- runState (g a)    :: s -> (b, s)
       -- runState (g a) s  :: (b, s)            OK

-- primitív State függvények:
--  állapot írása + olvasása

-- jelenlegi állapotot visszaadja mint érték
get :: State s s
get = State $ \s -> (s, s)   -- állapot nem változik

-- jelenlegi állapotba egy konkrét értéket írunk (értékadás)
put :: s -> State s ()
put s = State $ \_ -> ((), s)

p2 :: State Int Bool
p2 = do
  n <- get
  if n < 10 then do
    put (n + 20)
    return True
  else do
    put (n + 30)
    return False

-- futattás:
-- runState p2 :: Int -> (Bool, Int)      (kezdő állapotból értéket + végső állapotot ad meg)

-- runState p2 0 == (True,20)
-- runState p2 15 == (False,45)

-- kényelmi függvény:

-- függvényt alkalmaz az állapotra
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

evalState :: State s a -> s -> a      -- csak visszatérési értéket ad vissza
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)  -- csak a végső állapotot adja vissza

p3 :: State Int ()
p3 = do
  modify (+100)
  modify (+200)
  modify (*10)

p4 :: State Int ()
p4 = replicateM_ 100 (modify (+100))

label' :: Tree a -> Tree (a, Int)
label' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (a, n))
  go (Node l r) = do
    l <- go l
    r <- go r
    return (Node l r)

-- összefoglalás:
--   State s a : egy darab "s" típusú írható-olvasható változót tudunk használni
--     - mi van, ha több változót szeretnénk használni egyszerre?
--       - 1. állapot legyen tuple: State (s1, s2) a
--         - (van finomítás, advanced Haskell ("lencsék" használata))
--       - 2. IO, ST, STM: mutábilis referenciát lehet tetszőlegesen létrehozni
--            IO: lehet benne mutálni, nincs garancia semmire
--            ST: lehet benne mutálni, viszont műveletek futtatása tiszta függvényt eredményez
--                (mutáció privát, kifelé nem látszik)
--                (keresés "ST Monad Haskell")
