{-# language DeriveFunctor #-}

{- - Applicative, műveletek, (példák: mapM, forever, filterM, etc), default instance-ok
   - ((,) a)  Applicative
   - Lista monád, comprehension, sublists
-}

-- Functor => Applicative => Monad

--------------------------------------------------------------------------------

{-# language DeriveFunctor #-}

import Control.Monad

newtype State s a = State {runState :: s -> (a, s)} deriving Functor
-- {-# language DeriveFunctor #-}

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


-- Applicative
--------------------------------------------------------------------------------

{-
"köztes" funkcioiniálitás Functor és Monad között.

  - Functor: mappelés
  - Monad: szekvenciális mellékhatásos programozás
  - Applicative: ?

standard:

  class Functor f => Applicative f where
    pure :: a -> f a
    -- kiejtése "ap"
    (<*>) :: f (a -> b) -> f a -> f b

Röviden: Applicative N-áris fmap-et támogat.
-}

l1 :: [Int]
l1 = fmap (+10) [0..10]

m1 :: Maybe Int
m1 = fmap (+10) (Just 0)  -- Just 10

-- 2-paraméteres függvény Maybe-n
-- (ha csak fmap-et és (+)-t használhatok, akkor ez nem definiálható!)
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just n) (Just n') = Just (n + n')
addMaybe _ _ = Nothing

-- általánosan (nem működik)
-- addF :: Functor f => f Int -> f Int -> f Int
-- addF fn1 fn2 = _

addMaybe' :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe' mn mn' = do
  n <- mn
  n' <- mn'
  return (n + n')

{-
instance Applicative Maybe where
   pure = Just        (ugyanaz mint a return)

   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
   Just f <*> Just a = Just (f a)
   _      <*> _      = Nothing


Törvények? Akit érdekel, Control.Applicative-ban elolvashatja (nem rész anyagnak).

- pure/return: historikus okok miatt szerepel ugyanaz a függvény kétszer a
  Functor=>Applicative=>Monad hierarchiában.
- pure és return teljesen ugyanaz (különböző pure/return definíció: hibás!)
- Régen nem volt Aplicative superclass-ja a Monad-nak
- Régen nem is létezett az Applicative (kb 2007 körül kezdték el használni).
  A Monad 90-es évek közepén terjedt el.

- én személyes konvencióm: *pure*-t használom return helyett
  (a pure típusa általánosabb mint a return-é)

-}

addMaybe'' :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe'' mn mn' = pure (+) <*> mn <*> mn'
                 -- (pure (+) <*> mn) <*> mn'
  -- pure (+)                  -- Maybe (Int -> Int -> Int)  (== Just (+))
  -- pure (+) <*> mn           -- :: Maybe (Int -> Int)
  -- pure (+) <*> mn <*> mn'   -- Maybe Int

-- tisztán:                                  ((+) x) y
-- Maybe Applicative függvényalkalmazás      (pure (+) <*> mx <*> my)

-- bináris fmap:
-- Control.Applicative-ban definiált:
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = pure f <*> fa <*> fb

-- liftA2 (+) (Just 0) (Just 10) == Just 10

-- unáris fmap (ugyanaz, mint az fmap)
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f fa = pure f <*> fa

-- 3 paraméter
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f fa fb fc = pure f <*> fa <*> fb <*> fc

-- 0 paraméter: (pure)
-- (szimpla értékre gondolhatok, mint 0 paraméteres függvény)
liftA0 :: Applicative f => a -> f a
liftA0 a = pure a

-- Applicative "kombinátor"
--   (kombinátor: magasabbrendű fv. szinonímája általában)

-- (házi feladat: Control.Applicative-beli függvényeket megnézni)
-- (fmap szinonímája mint operátor)

-- standard:
-- (<$>) = fmap

-- N-áris fmap-hez praktikus:
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb =
  f <$> fa <*> fb    -- fmap f <*> fb

  -- gyakorlatban ezt a formát használják
  -- N-áris fmap általánosan:
  -- f <$> fa <*> fb <*> fc <*> ...... <*> fn

-- (*>) :: Applicative f => f a -> f b -> f b
-- azonos a (>>)-al (Monad m => m a -> m b -> m b)
--   konstans bind, ami egymás után elvégez két műveletet

-- alkalmazzuk a bináris fmap-et:
-- (*>) fa fb = (\a b -> b) <$> fa <*> fb

-- (<*) :: Applicative f => f a -> f b -> f a
-- (<*) fa fb = (\a b -> a) <$> fa <*> fb

-- "konstans fmap"
-- (<$) :: Functor f => a -> f b -> f a
-- (<$) a fb = (\_ -> a) <$> fb

-- példa alkalmazásra:

io1 :: IO String
io1 = do
  l1 <- getLine
  l2 <- getLine
  return (l1 ++ l2)

io1' :: IO String
io1' = (++) <$> getLine <*> getLine

-- olvasunk egy sort, nyomtatunk üzenetet, sort adjuk vissza
io2 :: IO String
io2 = getLine <* putStrLn "hello"

-- egymás után több művelet végrehajtása, viszont azt az értéket adjuk vissza, amire a csőr mutat

-- olvassunk 4 sort, adjuk vissza a harmadikat
io3 :: IO String
io3 = getLine *> getLine *> getLine <* getLine <* putStrLn "hello"

io4 :: IO ()
io4 = () <$ io3  -- (() <$ x)  :  dobjuk el egy művelet visszatérési értékét
                 -- (akkor használjuk, ha nem vagyunk kíváncsiak a visszatérési értékre)

--------------------------------------------------------------------------------
-- Milyen művelet definiálható monádikusan, viszont *nem* definiálható Applicative
-- kombinátorokkal?

-- Applicative definiálható művelet: - nem interaktív
--                                   - nem függhetnek a műveletek a visszatérési értékektől
--                                   - statikus ismertek hatások és a sorrendjük

-- Monádikus művelet: - tetszőlegesen interaktívak
--                    - visszatérési értéktől függ a későbbi mellékhatás

-- példa: *nem* Applicative
io5 :: IO ()
io5 = do
  l <- getLine
  if (length l < 5)
    then putStrLn "next" >> io5
    else putStrLn "bye"

-- korábbi függvények, amelyeket Monad-ban definiáltunk, viszont valójában
-- Applicative-ak (nem interaktív)

-- példa: mapM
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (a:as) = (:) <$> f a <*> mapA f as

-- mapM f [] = return []
-- mapM f (a:as) = do
--   b <- f a
--   bs <- mapM f as
--   return (b:bs)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n fa | n <= 0 = pure []
replicateA n fa = (:) <$> fa <*> replicateA (n - 1) fa

--------------------------------------------------------------------------------

-- Milyen haszna lehet Applicative függvényeknek?
--    statikusan elemezni az Applicative műveleteket
--    gyakorlatban pl: Applicative, ahol a mellékhatás adatbázis lekérdezés (megszorítása az IO monádnak)
--    mivel az Applicative-ban statikusan ismertek a hatások
--       lehetséges olyan optimalizáció az instance-ban, ami monádikusa API-nál lehetetlen
--       batch-elés, optimalizálni, stb.
--    (runQuery :: Query a -> IO a)  -- kb így néz ki egy futtató függvény
--    Library: Haxl (Facebook)

--------------------------------------------------------------------------------

-- Példa: Applicative, de nem Monad

data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

-- pár első mezője "súly" annotáció, ahol mindennek van súlya, és a súlyok összeadódnak
instance Monoid a => Applicative (Pair a) where

  -- 0 súlyú művelet
  pure a = Pair mempty a
  -- kombinált művelet
  Pair a f <*> Pair a' x = Pair (a <> a') (f x)

-- standard instance: Monoid a => Applicative ((,) a)
-- példa: (+) <$> ("bar", 10) <*> (" xx", 30) == ("bar xx",40)


-- Lista monád
--------------------------------------------------------------------------------

-- instance Monad [] where
--   -- return :: a -> [a]
--   return a = [a]
--   -- (>>=) :: [a] -> (a -> [b]) -> [b]
--   as >>= f = concatMap f as

-- "concat" listákra
-- viszont: minden Monad-ra van concat függvény

-- import Control.Monad
join :: Monad m => m (m a) -> m a
join mma = mma >>= \ma -> ma

-- pl Maybe join
-- IO-ban join?
-- join :: IO (IO a) -> IO a   (először a külső, aztán a belső lefut)

-- (vehetjük a join függvényt is Monad metódusnak, (>>=) helyett)
-- (join és (>>=) egymásból mindig definiálhatók)
-- (házi feladat: hogy lehet join-ból (>>=)-ot csinálni)
-- (alternatív leírás: matematikában a join gyakrabban használt alapművelet)
