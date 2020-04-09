{-# language DeriveFunctor #-}

import Control.Monad (ap)

-- Monad folyt, State
--------------------------------------------------------------------------------

-- Monad class

-- superclass megszorítás
-- pl: class Eq a => Ord a

-- konkrétan:

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- class Applicative m => Monad m where
--   return :: a -> m a                    -- (ugyanaz, mint a pure, historikus okból
--                                         -- létezik)
--   (>>=) :: m a -> (a -> m b) -> m b

-- Functor => Applicative => Monad (4 darab metódus)

-- Monad instance implementálásánál meg kell adnunk mind a 3 instance-ot

-- példa: Maybe monad

data Maybe' a = Nothing' | Just' a
  deriving Show  -- opcionálisan: deriving (Functor)

instance Functor Maybe' where
  fmap f ma = ma >>= \a -> return (f a)                  -- Maybe' b

instance Applicative Maybe' where
  pure = return
  mf <*> ma =        -- (<*>) kiejtése "ap"
    mf >>= \f ->
    ma >>= \a ->
    return (f a)

instance Monad Maybe' where
  return = Just'
  Just' a  >>= f = f a
  Nothing' >>= _ = Nothing'

-- elég csak a Monad metódusokat megadni, a többi levezethető

-- fmapDefault :: Monad m => (a -> b) -> m a -> m b
-- fmapDefault f ma = ma >>= \a -> return (f a)

-- tanulság, hogy elég Monad instance-ot megadni, a Functor és Applicative instance
-- adódik a fenti kód kimásolásával.


-- Monad törvények (egyenlőségek)
------------------------------------------------------------

-- korábban:
-- fmap id x = x
-- fmap (f . g) x = fmap f (fmap g x)

-- Monád törvények:
-- segédfüggvény törvények definiálásához: monádikus függvénykompozíció

-- std kompozíció: (.)   :: (b -> c) -> (a -> b) -> (a -> c)      -- (f . g) x = f (g x)
-- monádikus     : (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a = f a >>= g
infixr 1 >=>

-- törvények:

-- 1. return-nek nincs mellékhatása, return az egységeleme a >=>-nak
--    (return >=> f) = f                -- return :: a -> m a
--    (f >=> return) = f

-- 2. (>=>) asszociatív
--    ((f >=> g) >=> h)  =  (f >=> (g >=> h))


-- intuitívan:
--    return-nek nincs mellékhatása
--    asszociativitás: csak a hatások *sorrendje* számít (hatások listája)

-- Monad: minimális elvárás (interface) a szekvenciális, mellékhatásos programokra

-- void f(int a){
--    {
--      printf("foo");
--      printf("bar");
--    }
--    ptrinf("baz");
-- }

-- nagyon alap elvárás
-- (valóságban nyelvekben nem mindig teljesül a monád törvény!)
--   (exception-ök, aszinkron programozás, konkurens prog)
--   (Javascript-ben aszinkron hívások: *majdnem* monádikus
--     (bind/return definiálható), de valami gáz mégis volt)
--   google (Javascript async monad)

f1 :: Int -> Maybe Int
f1 = Just >=> (\a -> if a < 0 then Nothing else Just a) >=> Just

f2 :: Int -> Maybe String
f2 = f1 >=> f1 >=> (\a -> return (show a))

-- f1 10 == Just 10


-- State monád
--------------------------------------------------------------------------------

-- State s a    (két paraméteres típus)
--              (s paraméter: írható-olvasható változó típusa)
--              (a paraméter: visszatérési típusa a monádnak)

-- (pontosan *egy* darab mutable változó használható!)
--   (deklarálhatunk és inicializálhatunk pontosan egy darab var-t)

-- nézzunk először egy motiváló feladatot:

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Functor)

-- számozzuk be balról jobbra az összes levelet

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (go t 0) where

  go :: Tree a -> Int -> (Tree (a, Int), Int)
  go (Leaf a)   n = (Leaf (a, n), n + 1)
  go (Node l r) n = case go l n of
    (l', n') -> case go r n' of
      (r', n'') -> (Node l' r', n'')

-- példa működésre:
--    labelTree (Node (Leaf True) (Leaf False)) == Node (Leaf (True, 0)) (Leaf (False, 1))

-- (ha kinyomtatunk egy fát, akkor a leveleke balról jobbra nyomtatásának sorrendjében
--  számozunk)

-- minden hívás explicit extra bemenetet kap és kimenetet ad
-- kézzel kell továbbadni a változót

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s -> case g s of (a, s) -> (f a, s)
    -- f :: a -> b
    -- g :: s -> (a, s)
    -- szeretnénk :: s -> (b, s)    (State belsejében)

  -- (visszaadott "a" értékre alkalmazunk egy további függvényt)

instance Applicative (State s) where
  pure  = return
  (<*>) = ap          -- Control.Monad import, monad instance-ból megadja (<*>)-t.

instance Monad (State s) where
  -- return :: a -> State s a
  -- nincs mellékhatás: s értéke nem változik:
  return a = State $ \s -> (a, s)

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- állapotot változtató függvények kompozíciója
  State f >>= g = State $ \s -> case f s of
    (a, s') -> (runState (g a)) s'
       -- g a :: State s b
       -- runState (g a) :: s -> (b, s)


-- API állapot módosítására:

-- két alap függvény:
-- put : beállítja az állapotot valamilyen értékre
-- get : olvassa a jelenlegi állapotot

put :: s -> State s ()         -- nem érdekes a visszatérési érték
put s = State $ \_ -> ((), s)  -- eldobjuk a jelenlegi állapotot, s-et visszaadjuk

-- jelenlegi állapotot *visszatérési értékként* adja vissza
get :: State s s
get = State $ \s -> (s, s)

g1 :: State Int ()
g1 = get >>= \n ->
     if n < 0 then put (n + 10)
              else put (n + 20)

-- használat példa: runState g1 10   == ((), 30)
--                  runState g1 (-1) == ((), 9)

-- State s a - típusú műveletben get és put használható
-- runState - nél meg kell adni a *kezdő* állapotot

-- alkalmaz egy függvényt a jelenlegi állapotra
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)


g2 :: State Int Int
g2 = modify (+10) >>
     modify (+20) >>
     modify (*2)  >>
     return 100

  -- s += 10;
  -- s += 20;
  -- s *= 2;
  -- return 100

-- runState g2 0 == (100, 60)

g3 :: State Int Int
g3 = modify (+10) >>
     modify (+20) >>
     modify (*2)  >>
     get >>= \n ->
     return (n - 10)

-- listaműveletek (push/pop)

push :: a -> State [a] ()
push a = modify (a:)

-- (lehet, hogy üres az állapot lista)
pop :: State [a] (Maybe a)
pop = get >>= \as ->
      case as of
        []   -> return Nothing
        a:as -> put as >> return (Just a)

-- runState (push 2 >> push 2) []  == ((), [2, 2])
-- runState (push 2 >> pop) [] == (Just 2,[])


------------------------------------------------------------


labelTree' :: Tree a -> Tree (a, Int)
labelTree' t = fst (runState (go t) 0) where

  go :: Tree a -> State Int (Tree (a, Int))
  go (Leaf a)   =
    get >>= \n ->
    put (n+1) >>
    return (Leaf (a, n))
  go (Node l r) =
    go l >>= \l' ->
    go r >>= \r' ->
    return (Node l' r')

-- (lesz ez még sokkal szebb is, pl Applicative és Traversable-el)
-- (a kód jelentős része derive-olható lesz)
