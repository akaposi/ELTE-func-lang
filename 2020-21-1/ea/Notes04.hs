
import Control.Monad (ap)

-- Monád törvények
--------------------------------------------------------------------------------

-- class Functor m => Monad m where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- instance Monad Maybe where
--   Nothing >>= f = Nothing
--   Just a  >>= f = f a
--   return = Just

-- Monad törvények?

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

-- "fish", monádikus kompozíció
-- tiszta kompozíció    : (.)   ::            (b -> c)   -> (a -> b)   -> a -> c      (\f g x -> f (g x))
-- monádikus kompozíció : (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--                        (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

infixr 4 >=>
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a = f a >>= \b -> g b
-- (f >=> g) a = f a >>= g

-- pl:
f1 :: Int -> Maybe Int
f1 = \x -> if even x then Just (x + 10) else Nothing

f2 :: Int -> Maybe Int
f2 = f1 >=> f1 >=> f1  -- iteráció + hibalehetőség

-- Monad törvények:
--   1,2 törvény: return mellékhatás-mentes
--   3   törvény: bind asszociatív (szekvencialitás: do blokkban mindegy, hogy hogyan csoportosítjuk az utasításokat,
--                                  hatások "listája"

-- (Extra return elhagyható, következmény: return-nek nem lehet mellékhatása!)
-- 1: return >=> f = f               (f :: a -> m b) tehát ((return >=> f) :: a -> m b)
-- 2: f >=> return = f

-- (Szekvencialitás, mindegy az utasítások csoportosítása)
-- 3: f >=> (g >=> h) = (f >=> g) >=> g

-- 3 következménye:

-- do ma                           do ma
--    do mb      ugyanaz mint         mb
--       mc                           mc


-- Házi feladat: Maybe esetén teljesül-e 1-3?


-- Néhány további monádikus függvény:
--------------------------------------------------------------------------------

-- replicate :: Int -> a -> [a]

-- hajtsunk végre valamit n-szer, gyűjtsük össze az eredményeket
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma | n <= 0    = return []
                | otherwise = do
                    a <- ma
                    as <- replicateM (n - 1) ma
                    return (a:as)

-- replicateM 10 (putStrLn "hello")

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n ma | n <= 0    = return ()
                 | otherwise = ma >> replicateM_ (n - 1) ma

-- replicateM_ 10 (putStrLn "hello")

-- (ghci-be bármit írunk, az olyan, mint egy IO do blokk)
-- pl: strings <- replicateM 3 getLine
--     (beolvas három sort, "strings" a három sor listája)


-- (jelenség: típusok tetszőleges lehetnek, ha nem térünk vissza értékkel/nem tárolunk értéket)
-- newtype Const a b = Const a
--   Const :: a -> Const a b    (mivel nem tárol b-t, ezért az tetszőleges)

-- végtelen loop (rekurzív)
-- mivel nem tér vissza, ezért a típusa tetszőleges
loop :: a
loop = loop

-- végtelenül ismétli a műveletet
forever :: Monad m => m a -> m b     -- (mivel nem tér vissza, ezért bármivel visszatér)
forever ma = ma >> forever ma

-- IO példa:
io1 :: IO ()
io1 = forever $ do
  line <- getLine
  putStrLn line
  putStrLn line

-- (ghci-ben Ctr-c-c megszakítja)
-- Control.Monad


-- State monád
--------------------------------------------------------------------------------

-- motiváció: írható/olvasható referencia (mutáció)

-- struktúra bejárás + információ nyilván tartása
-- példa először: State monád nélkül

-- bináris leveles fa
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- balról jobbra (bejérási sorrendben) beszámozni a Leaf-eket
-- legbal Leaf: 0 címkéje, stb.

-- példa: label (Node (Leaf True) (Node (Leaf False) (Leaf True)))
--         == (Node (Leaf (True, 0)) (Node (Leaf (False, 1)) (Leaf (True, 2))))

label :: Tree a -> Tree (a, Int)   -- + Int a címke
label t = fst (go t 0) where  -- címke nyílván tartása szükséges!

  -- extra Int bemenet + extra Int kimenet!  (Int "állapot", változik az állapot)
  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a)   n = (Leaf (a, n), n + 1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of          -- extra paraméter továbbadása
      (r', n'') -> (Node l' r', n'')

-- házi feladat: label függvény, viszont:
-- data Tree a = Node a [Tree a]
--   (kézi Int továbbadás nagyon csúnya!)

-- ehelyett írjunk Monad instance-ot

-- s: bemenő állapot + kimenő állapot
-- a: visszatérési értéke s-től függő műveletnek

newtype State s a = State {runState :: s -> (a, s)} -- szintaxis: rekord mező szintaxis
                                                    -- runState :: State s a -> (s -> (a, s))
                                                    -- State    :: (s -> (a, s)) -> State s a

-- superclass hierarchia: Functor => Applicative => Monad

instance Functor (State s) where
  fmap f (State g) = State (\s -> case g s of (a, s') -> (f a, s'))

-- ki is lehet ezt hagyni, mivel "automatikusan" implementálható a Monad instance segítségével
-- import Control.Monad
instance Applicative (State s) where
  pure  = return
  -- "ap"
  (<*>) = ap

instance Monad (State s) where
  -- return :: a -> m a
  -- return :: a -> State s a
  return a = State (\s -> (a, s))   -- változatlanul továbbadja az s-et (állapotot)

  -- állapot propagálja
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')
  -- g :: a -> State s b
  -- newtype nélkül: a -> s -> (b, s)

-- szükség van: API az "s" írás/olvasáshoz magasabb szintű műveleteket


-- állapot lekérdezése :
get :: State s s  -- lekérdezi az állapotot, visszaadja értékként
get = State (\s -> (s, s))

-- írás: állapotot írjuk konkrét értékre
put :: s -> State s ()
put s = State (\_ -> ((), s))   -- bemenő állapotot eldobjuk, helyette s-t továbbadjuk


-- példa: list push/pop művelet, állapot egy lista
--        műveletek listát módosítanak

push :: a -> State [a] ()
push a = do
  as <- get    -- get :: State [a] [a]
  put (a:as)

-- pop Nothing-ot ad vissza, ha a lista üres
-- egyébként visszaadja az első elemet + állapotot változtat (leszedi az elemet)
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []    -> return Nothing
    a:as' -> do
      put as'
      return (Just a)

-- imperatív kód, ahol put/get *egyetlen* s-típusú mutálható változót ír/olvas

-- var list = ....;

-- def pop():
--   case list of
--     []    -> return Nothing
--     a':as -> list := as; return (Just a')

-- példa:
f3 :: State [Int] ()
f3 = do
  push 0
  push 2
  push 4

-- használat: runState
-- runState f3 [] == ((), [4, 2, 0])
-- runState (replicateM_ 20 (push 10)) [] ==
--     ((),[10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10])


-- módosítsuk az állapotot egy függvényel
modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

-- push a = modify (a:)

-- runState, viszont csak a végeredményre vagyunk kíváncsiak
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

-- csak a végső állapot érdekes
execState :: State s a -> s -> s
execState ma s = snd (runState ma s)


-- példa
label' :: Tree a -> Tree (a, Int)   -- + Int a címke
label' t = evalState (go t) 0 where

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a) = do
    n <- get
    put (n + 1)
    return (Leaf (a, n))
  go (Node l r) = do
    l' <- go l            -- ha fordított sorrendet akarunk, csak megcseréljük a két sort
    r' <- go r            --
    return (Node l' r')

  -- go :: Tree a -> Int -> (Tree (a, Int), Int)
  -- go (Leaf a)   n = (Leaf (a, n), n + 1)
  -- go (Node l r) n = case go l n of
  --   (l', n') -> case go r n' of
  --     (r', n'') -> (Node l' r', n'')
