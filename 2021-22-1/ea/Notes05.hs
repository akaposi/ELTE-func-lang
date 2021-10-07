{-# language InstanceSigs #-}

import Control.Monad  -- (ap)

-- 5. EA
-- State, tree, push/pop példák
-- Applicative intro

------------------------------------------------------------

-- (standard: Control.Monad.State)
newtype State s a = State {runState :: s -> (a, s)}

-- State    :: (s -> (a, s)) -> State s a
-- runState :: State s a -> (s -> (a, s))        -- runState (st :: State s a) s :: (a, s)

-- type State s a = s -> (a, s)                  -- (instance-ok miatt kényelmesebb a newtype)
--

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

-- írjunk egy függvényt, bejárás sorrendjében 0-tól beszámozza a leveleket
--     (bejárás: balról-jobbra mélységi)

-- pl: label (Node (Node (Leaf True) (Leaf True)) (Leaf False))
--      == Node (Node (Leaf (True, 0)) (Leaf (True, 1))) (Leaf (False, 2))

countLeaves :: Tree a -> Int
countLeaves = undefined

{-
label' :: Tree a -> Int -> Tree (a, Int)
label' (Leaf a) n = Leaf (a, n)
label1 (Node l r) n = Node (label' l n) (label' r (countLeaves l))
    -- ahány Leaf van l-ben, arról az Int-ről kéne kezdeni r címkézését!
    -- (exponenciális komplexitás! Minden Node-nál kétszer bejárjuk l-t)
    -- megoldás: egy függvény megadja a fát is és a következő indexet is

    -- go :: Tree a -> Int -> (Tree (a, Int), Int)
    -- go :: Tree a -> State Int (Tree (a, Int))

    -- "Stateful" művelet: s -> (a, s)
-}

label :: Tree a -> Tree (a, Int)
label t = fst (go t 0) where
  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a) n = (Leaf (a, n), n + 1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor (State s) where
  -- futtatjuk a műveletet, a végeredményen f-et alkalmazzuk

  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> case g s of (a, s) -> (f a, s))
      -- State (\s -> _)      _ :: (b, s)
      -- g :: s -> (a, s)
      -- g s :: (a, s)
      -- case g s of (a, s) -> (f a, s)  :: (b, s)

-- Applicative instance-t mechanikusan definiáljuk a Monad instance-ból
instance Applicative (State s) where
  pure  = return
  (<*>) = ap

-- kitérő: superclass-ok levezethetősége
------------------------------------------------------------

-- Monad instance-ból Applicative levezethető
-- Applicative-ból a Functor instance levezethető
-- elég *csak* Monad instance-t definiálni, a t9bbi mechanikusan adódik

fmap' :: Monad m => (a -> b) -> m a -> m b
fmap' f ma = do
  a <- ma
  return (f a)

-- fmap' f ma =
--   ma >>= \a ->
--   return (f a)

-- Példa: superclass, viszont a (<>) *nem* tudjuk a mempty segítségével automatikusan megadni.
-- class Semigroup a where
--   (<>) :: a -> a -> a

-- class Semigroup a => Monoid a where
--   mempty :: a

------------------------------------------------------------

instance Monad (State s) where

  -- nincs mellékhatás: "s" nem változik
  return :: a -> State s a
  return a = State (\s -> (a, s))

  -- egymás után való végrehajtás
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) (State f) g = State $ \s -> case f s of
    (a, s) -> runState (g a) s

      -- g              :: a -> State s b
      -- g a            :: State s b
      -- runState (g a) :: s -> (b, s)

      -- ha nincs newtype, akko ez lenne:
      --   (a, s) -> g a s


--------------------------------------------------------------------------------

-- két State specifikus alapművelet:

-- state írása
put :: s -> State s ()            -- hatás: bemenő állapotot eldobjuk helyette "s" az új állapot
put s = State $ \_ -> ((), s)

-- state olvasás
get :: State s s                  -- hatás: a jelenlegi állapotot visszaadjuk mint érték
get = State $ \s -> (s, s)

-- std
-- (a végeredményre vagyunk kíváncsiak)
evalState :: State s a -> s -> a
evalState sta s = fst (runState sta s)

-- std
-- (állapotváltozásra vagyunk kíváncsiak)
execState :: State s a -> s -> s
execState sta s = snd (runState sta s)

label' :: Tree a -> Tree (a, Int)
label' t = evalState (go t) 0 where

  -- go t :: State Int (Tree (a, Int))
  -- runState (go t) :: Int -> (Tree (a, Int), Int)
  -- fst (runState (go t) 0) :: Tree (a, Int)

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get              -- n := state
    put (n + 1)           -- state := n + 1
    return (Leaf (a, n))
  go (Node l r) = do
    l' <- go l
    r' <- go r
    return (Node l' r')

-- fenti definíció: cukorka nélkül + definíciók kifejtése után
--   pontosan a korábbi label definíciót kapjuk
--   ghc -O1 opció (vagy -O2), akkor a fordított kimenet ugyanaz (optimalizálás eltűnteti az absztrakciót)

-- kézzel fordítás:
  -- go (Leaf a) =
  --   get >>= \n ->
  --   put (n + 1) >>= \_ ->
  --   return (Leaf (a, n))

  -- go (Leaf a) =
  --   State (\s -> (s, s))           >>= \n ->
  --   State (\_ -> ((), n + 1))      >>= \_ ->
  --   State (\s -> (Leaf (a, n), s))

  -- go (Leaf a) =
  --   State (\s -> (s, s))      >>= \n ->
  --   State (\_ -> ((), n + 1)) >>= \_ ->
  --   State (\s -> (Leaf (a, n), s))

  --   opcionális házi:

  --   State (\_ -> ((), n + 1)) >>= \_ ->
  --   State (\s -> (Leaf (a, n), s))
  --   ==
  --   State $ \_ -> (Leaf (a, n), n + 1)

  -- n <- get
  -- put (n + 1)
  -- return (Leaf (a, n))
  --  ==
  -- State $ \n -> (Leaf (a, n), n + 1)

--------------------------------------------------------------------------------

-- csak használjuk a State-et:
--  imperatív program, egy "s" típusú írható-olvasható változó


-- Applicative
--------------------------------------------------------------------------------

{-
-- std
class Functor f => Applicative f where
  pure  :: a -> f a                   -- konvenció: pure megegyezik a "return"-el
                                      --  historikus ok: miért van egyálatlán return? (ha a pure ugyanaz)
                                      --                 return kizárólag backwards kompatibilitás miatt létezik
                                      --                 tipp: return helyett mindig lehet "pure"-t használni

  (<*>) :: f (a -> b) -> f a -> f b   -- kiejtése "ap"

-- pure, (<*>) segítségével általánosítjuk az fmap függvényt,
--  nem csak 1 paraméteres függvénnyel tudunk map-elni, hanem N paraméteressel
-}

-- fmap: 1 paraméteres fv.
p1 :: IO Int
p1 = fmap length getLine
   -- getLine :: IO String
   -- length :: String -> Int

-- 2 sort olvasok, összefűzöm a két sort

-- Monad-al:
-- p2 :: IO String
-- p2 = do
--   l1 <- getLine
--   l2 <- getLine
--   pure (l1 ++ l2)

p2 :: IO String
p2 = (++) <$> getLine <*> getLine

   -- getLine          :: IO String
   -- (++)             :: String -> String -> String
   -- (++) <$> getLine :: IO (String -> String)

   -- (<*>) :: IO (a -> b) -> IO a -> IO b

   -- (++) <$> getLine <*> getLine  :: IO String
   -- ((++) <$> getLine) <*> getLine  :: IO String

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = f <$> fa <*> fb

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc = f <$> fa <*> fb <*> fc

-- stb...
-- fmapN f x_i = f <$> x_0 <*> ... <*> x_N


-- Applicative: nem muszáhj minden részeredményt bind-olni.
label'' :: Tree a -> Tree (a, Int)
label'' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (a, n))
  go (Node l r) = Node <$> go l <*> go r   -- imperatív pszeudokód: Node(go(l), go(r))


-- Applicative = N-paraméteres fmap
-- Mi a különbség Applicative és Monad között?

--    Applicative: statikusan (futtatás előtt) ismert minden mellékhatás
--    Monad      : csak futás során derül ki, hogy milyen hatás jön létre

-- példa:

p3 :: IO ()
p3 = do
  n <- length <$> getLine
  replicateM_ n (putStrLn "hello")   -- hatás függ a futás során kapott értéktől

-- csak Applicative metódus: *nincs* p3 definíció
--    Applicative-al definiálható művelet: jobban optimalizálható ("statikusan" ismert hatás)
--    példa: Facebook Haxl: spam-szűrő:  Applicative adatbázis-lekérdező (beágyazott) nyelv
--                                       mielőtt elküldjük a lekérdezést, optimalizáljuk (batch-eljük)

--
