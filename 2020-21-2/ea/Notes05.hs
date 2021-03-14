
import Control.Monad

-- Admin: első házit kiírom ma/holnap (4 pont) (téma: State monad feladat)

-- State, Applicative
--------------------------------------------------------------------------------


newtype State s a = State {runState :: s -> (a, s)}

-- Szimpla függőség:  a -> b        (b függ a-tól)
-- Állapot-függőség:  s -> (a, s)   (a függ s-től mint állapottól)

--     s          ->     (a, s)
-- input state    ->     (visszatérési érték, output state)


-- runState :: State s a -> (s -> (a, s))
-- runState :: State s a -> s -> (a, s)

-- State :: (s -> (a, s)) -> State s a  (runState inverze)


-- motiváció State s a használatára
--------------------------------------------------------------------------------

-- sok esetben mutálható állapot / írható referenciák a természetes eszköz

-- fa bejárás + info nyilvántartás

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)


-- feladat: Leaf-eket balról jobbra számozzuk be 0-tól kezdve
label :: Tree a -> Tree (Int, a)
label t = fst (go 0 t) where
  go :: Int -> Tree a -> (Tree (Int, a), Int)
  go n (Leaf a)   = (Leaf (n, a), n + 1)
  go n (Node l r) = case go n l of
    (l', n') -> case go n' r of
      (r', n'') -> (Node l' r', n'')

-- label (Node (Node (Leaf True) (Leaf False)) (Node (Leaf True) (Leaf False)))
--   ==
-- Node (Node (Leaf (0,True)) (Leaf (1,False))) (Node (Leaf (2,True)) (Leaf (3,False)))

-- emlékezzünk: Maybe bind

--   case e of
--     Nothing -> Nothing
--     Just x -> case e2 of
--       Nothing -> Nothing
--       Just y  -> ...

-- Functor, Applicative, Monad instance-ok
--------------------------------------------------------------------------------

instance Functor (State s) where
  fmap f (State g) = State $ \s -> case g s of (a, s') -> (f a, s')

  -- g :: s -> (a, s),   _ :: s -> (b, s)
  -- emlékezzünk: Functor ((->) a)

  -- tömören fmap State-re: művelet visszatérési értéke tovább módosul
  --                        (ugyanaz, mint az IO-ra!)
  --                        pl: fmap length getLine :: IO Int

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where

  -- minden Monad: a return-nek nincs hatása
  -- nulla hatás: s-et változatlanul továbbadjuk
  return a = State $ \s -> (a, s)

  -- továbbadjuk az s-t egyik műveletből a másikba
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- (>>=) :: (s -> (a, s)) -> (a -> s -> (b, s)) -> s -> (b, s)

-- get megadja a jelenlegi állapotot
-- put beállítja az állapotot egy értékre
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()        -- csak a mellékhatás érdekes
put s = State $ \_ -> ((), s)



-- runState: kap egy műveletet + kezdő állapotot,
--           visszaad egy értéket + végső állapotot

label' :: Tree a -> Tree (Int, a)
label' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (Int, a))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    pure (Leaf (n, a))
  go (Node l r) = do
    l' <- go l
    r' <- go r
    pure (Node l' r')

  -- fenti pontosan ugyanaz a program, mint a korábbi

-- praktikus összefoglaló:

-- State s a  -- program, amiben pontosan egy darab írható "s" típusú változó használható
-- put s      -- var := s
-- x <- get   -- x := var

-- egy függvény alkalmazása "s"-re
-- modify :: (s -> s) -> State s ()
-- modify f = State $ \s -> ((), f s)

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- runState :: State s a -> s -> (a, s)

-- ha csak a visszatérési érték érdekes:
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

-- ha csak a végső állapot érdekes
execState :: State s a -> s -> s
execState ma s = snd (runState ma s)


--
--------------------------------------------------------------------------------

-- állapot : [Int]   (stack)

push :: Int -> State [Int] ()
push n = modify (\ns -> n:ns)
      -- modify (n:)

-- State nélkül:
push' :: Int -> [Int] -> ((), [Int])
push' n ns = ((), n:ns)

-- általános tipp: ha elég szimpla függvény, nem kell State


-- pop: ha az állapot üres, akkor Nothing-ot ad, egyébként,
-- leveszi a fejelemet az állapotról, és Just-ban visszaadja
pop :: State [Int] (Maybe Int)
pop = do
  ns <- get
  case ns of
    []   -> pure Nothing
    n:ns -> do
      put ns
      pure (Just n)

pop' :: [Int] -> (Maybe Int, [Int])
pop' = \ns ->
  case ns of
    []   -> (Nothing, ns)
    n:ns -> (Just n, ns)


-- lépésenként kifejtés:

-- push 10 >> push 10
-- push 10 >>= \_ -> push 10
-- modify (10:) >>= \_ -> modify (10:)

-- = State $ \s -> case (\s -> ((), 10:s)) s of
--      (a, s') -> runState ((\_ -> (State $ \s -> ((), 10:s))) a) s'

-- = State $ \s -> case ((), 10:s) of
--      (a, s') -> runState (State $ \s -> ((), 10:s)) s'

-- = State $ \s -> runState (State $ \s -> ((), 10:s)) (10:s)

-- = State $ \s -> (\s -> ((), 10:s)) (10:s)

-- = State $ \s -> ((), 10:10:s)


-- műveleteket lefordítottam lambda + pár + case kifejezésre
-- minden =-el definiált dolgot be lehet helyettesíteni, és
--   a program viselkedése nem változik
------------------------------------------------------------

-- GHC -O1 fordítja (push 10 >> push 10)        (nincs futásidejű extra költség)


-- házi feladat : kifejteni (lefordítani) következőket:

-- pop
-- push 10 >> pop
-- replicateM_ 3 pop        (Control.Monad.replicateM_)


-- Applicative
--------------------------------------------------------------------------------

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b       -- függvény app működik f-en belül

-- példák:
--   fmap-et kiterjesztjük N paraméterre

--  fmap :: (a -> b) -> f a -> f b
--  ?    :: (a -> b -> c) -> f a -> f b -> f c  (ha Functor f, akkor nem tudjuk, hogy van-e)
--
-- (<*>) függvényből minden N-paraméteres fmap levezethető

fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2 f fa fb = pure f <*> fa <*> fb

   -- pure f :: f (a -> b -> c)
   --     fa :: fa
   --  pure f <*> fa :: f (b -> c)
   --  pure f <*> fa <*> fb :: f c

-- tömörebb def (standard forma)
-- fmap szinonímája, mint operátor (<$>)


fmap2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2' f fa fb = f <$> fa <*> fb

  -- zárójelezés: (f <$> fa) <*> fb
  --   f               :: (a -> b -> c)
  --   fa              :: f a
  --   fmap f fa       :: f (b -> c)
  --   f <$> fa        :: f (b -> c)
  --   f <$> fa <*> fb :: f c

-- példák: N-paraméteres fmap-elés:

io1 :: IO Int
io1 = (+) <$> (length <$> getLine) <*> (length <$> getLine)

append3 :: [a] -> [a] -> [a] -> [a]
append3 xs ys zs = xs ++ ys ++ zs

io2 :: IO String
io2 = append3 <$> getLine <*> getLine <*> getLine

-- imperatív megfelelő:

{-
f() {
  var x = g(h1(10), h2());       x <- g <$> h1 10 <*> h2
-}

-- Kérdés: miért különböztetjük meg egyáltalán Applicative-ot Monad-tól?
-----------------------------------------------------------------------

-- Válasz: Applicative kifejez *nem interaktív* mellékhatásos programokat
--         Monad               *interaktív* programokat reprezentál

-- Applicative program: hatékonyan elemzhető futtatás nélkül
--     (futtatás nélkül látszik, hogy milyen hatások történnek)

-- Monadikus program: *nem* látszik statikusan, hogy milyen hatások jönnnek létre


------------------------------------------------------------
