

import Control.Monad -- ap

newtype State s a = State {runState :: s -> (a, s)}   -- 1 konstruktor, 1 mező


-- feladat 1: írd meg a következő instance-t
instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> case g s of (a, s) -> (f a, s))

-- emlékeztető:
newtype Fun a b = Fun (a -> b)
instance Functor (Fun a) where
  fmap f (Fun g) = Fun (\a -> f (g a))
  -- fmap f (Fun g) = Fun (f . g)


-- feladat 2: definiáld a következő függvényeket.
-- A lényeg, hogy típushelyes legyen a megoldás, rossz definíciót írni
-- tudomásom szerint nem lehet (kivéve loop/undefined)
get :: State s s
get = State $ \s -> (s, s)
   -- State $ \s -> (undefined, undefined)
-- get = get  (loop)

-- get: nem módosítja az állapotot, de visszaadja értékként is
-- State s s : a visszatérési típus megegyezik az állapot típusával

returnState :: a -> State s a
returnState a = State $ \s -> (a, s) -- csak egy megoldás
-- nem módosít állapotot, visszaad egy értéket

-- feladat 3: írj két-két *különböző* definíciót a következő függvényekhez!
evalState :: State s a -> s -> a
evalState (State f) s = case f s of (a, s') -> a
-- evalState (State f) s = case f s of (a, s') -> case f s' of (a, s'') -> a

execState :: State s a -> s -> s
execState (State f) s = snd (f s)
-- execState (State f) s = s
-- execState (State f) s = ((snd . f) . (snd . f) . (snd . f) . (snd . f) . (snd . f) . (snd . f) . (snd . f)) s

-- pontosan 2 lehetséges def
put :: s -> State s ()
put s = State $ \_ -> ((), s)
-- put s = State $ \s' -> ((), s')
-- put: kezdő állapotot eldobjuk, új állapot valamilyen adott "s" érték

-- State függvény értelmezés
-- State $ \s -> (exp1, exp2)
--   olyan művelet, ami "s" kezdőállapotból exp1 értéket ad vissza és
--   exp2-re módosítja az állapotot

--
------------------------------------------------------------

-- (>>) :: Monad m => m a -> m b -> m b

constBindState :: State s a -> State s b -> State s b
constBindState (State f) (State g) =
  State $ \s -> case f s of (a, s') -> g s'

-- extra feladat:
bindState :: State s a -> (a -> State s b) -> State s b
bindState (State f) g = State $ \s -> case f s of
  (a, s') -> case g a of
    State g' -> g' s'
  -- vagy:
  -- (a, s') -> runState (g a) s'

------------------------------------------------------------

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return = returnState
  (>>=)  = bindState

------------------------------------------------------------

-- list push/pop
push :: a -> State [a] ()
push a = do
  as <- get
  put (a:as)

-- monad instance nélkül:
push' :: a -> State [a] ()
push' a = State $ \as -> ((), a:as)

-- leszedünk egy értéket a listáról, ha nem üres a lista
-- ha üres a lista, visszaadjuk azt, hogy Nothing
pop :: State [a] (Maybe a)
pop = do
  as <- get
  case as of
    []   -> pure Nothing
    a:as -> do                    -- as árnyékolja a másik as-t
      put as                      -- (én ilyen esetben szándékoson árnyákolok)
      pure (Just a)

-- monad instance nélkül:
pop' :: State [a] (Maybe a)
pop' = State $ \as -> case as of
  []   -> (Nothing, [])
  a:as -> (Just a, as)


-- Következő órai bead:
--   függvény két módon:
--     1. State művelet csak monad metódus + put + get
--     2. State művelet csak State konstruktorral, monad nélkül
