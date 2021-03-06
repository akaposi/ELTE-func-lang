
import Control.Monad (ap)

-- Maybe ismétlés, generikus Monad függvények: mapM, sequence
-- do notáció
-- IO
-- további generikus: replicateM, forever, filterM
-- Monad törvények
-- State monad

-- motiváció: Maybe: hibakódos hibakazelés (sok zaj)
--                   catch/throw jellegű stílusban szeretnénk programozni,
--                   háttérben még mindig Maybe van

-- instance Monad Maybe        -- Monad: custom mellékhatást definiál
--                             -- Maybe: mellékhatás = Nothing propagálás mint kivétel


-- Just :: a -> Maybe a
-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just a) f = f a

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case mapMaybe f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe' f []     = Just []
mapMaybe' f (a:as) =
  bind (f a)           $ \b ->
  bind (mapMaybe f as) $ \bs ->
  Just (b:bs)

-- "monádikus" forma

-- sima függvény komp: (.) :: (b -> c) -> (a -> b) -> a -> c
--                     (.) f g x = f (g x)

-- monádikus kompozíció (std általános függvény is ilyen sorrendben van)
compMaybe' :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
compMaybe' f g a = case f a of
  Nothing -> Nothing
  Just b  -> g b

compMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
compMaybe f g a = bind (f a) g

--------------------------------------------------------------------------------

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b    -- ap

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b   -- ez a bind

-- return ugyanaz mint a pure (konvenció: return definíciója ugyanaz, mint pure-é)
--     (historikus ok)
--     (én személye szerint: csak a pure-t használom, a return-t)


-- instance Functor Maybe where
--   fmap f Nothing  = Nothing
--   fmap f (Just a) = Just (f a)

-- instance Applicative Maybe where
--   pure  = return
--   (<*>) = ap

-- instance Monad Maybe where
--   return = Just
--   (>>=)  = bind

-- ha van Monad instance --> Applicative triviális
-- ha van Applicative instance --> Functor triviális

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap = fmapFromMonad

instance Applicative Maybe' where
  pure  = return
  (<*>) = ap

instance Monad Maybe' where
  return = Just'
  Nothing' >>= f = Nothing'
  Just' a  >>= f = f a

-- (>>=) :: m a -> (a -> m b) -> m b
-- pure/return :: a -> m a
fmapFromMonad :: Monad m => (a -> b) -> m a -> m b
fmapFromMonad f ma = ma >>= \a -> pure (f a)
       -- f a :: b

--------------------------------------------------------------------------------

-- standard: mapM

-- monádikus map listákra
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = pure []    -- tiszta értékre mindig gondolhatunk úgy,
                        -- mint mellékhatás-mentes műveletre
mapM' f (a:as) =
  f a        >>= \b ->
  mapM' f as >>= \bs ->
  pure (b:bs)

-- lista "szekvenciálás"
-- műveletek listája --> listát visszaadó művelet
sequence' :: Monad m => [m a] -> m [a]
sequence' mas = mapM' (\ma -> ma) mas   -- mapM' id


-- konstans bind: egymás után végrehajtunk két műveletet, második nem függ
-- az első visszatérési értékétől
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb


-- IO monád
--------------------------------------------------------------------------------

-- instance Monad IO
-- (beépített dolog)

-- p :: IO a     -- p egy IO hatással járó program, ami "a" típusú értékkel tér vissza


-- main függvény:
--  () standard típus, kiejtése "unit"
--  data One = One
--  data () = ()        -- () szintaktikus cukorka


-- print :: Show a => a -> IO ()
--

-- main :: IO ()           -- IO mellékhatás + triviális értékkel tér vissza
-- main = print 100 >> print True >> print [0..10]


-- getLine :: IO String         -- egy sort beolvas stdin-ról
-- putStrLn :: String -> IO ()  -- printel egy String-et, newline a végére

-- main :: IO ()
-- main =
--   getLine    >>= \l ->
--   putStrLn l >>
--   putStrLn l

-- do notation:

main :: IO ()
main = do
  l <- getLine              -- var l = getLine();
  putStrLn l
  putStrLn l


-- do
--   1 vagy több sor
--   minden sor:
--   x <- rhs
--   p1
--   p2

-- példák a fordításra:

-- do
--   x <- rhs           rhs >>= \x ->
--   p                  p


-- do
--   p1                 p1 >>
--   p2                 p2


main' :: IO ()
main' = do {l <- getLine; putStrLn l; putStrLn l}


-- Maybe
mapMaybe'' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe'' f []     = pure []
mapMaybe'' f (a:as) = do
  b  <- f a
  bs <- mapMaybe f as
  pure (b:bs)

-- (ghci-ben: ha (IO a) értéket írunk be, azt le is futtatja)
-- mapM verziója, amikor a viszatérési érték nem érdekes, csak a hatás
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' f []     = pure ()
mapM_' f (a:as) = f a >> mapM_' f as


-- Control.Monad modulból importálhatók:

-- monadikus replicate
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma | n <= 0 = pure []
replicateM n ma = do
  a  <- ma
  as <- replicateM (n - 1) ma
  pure (a:as)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ = undefined

forever :: Monad m => m a -> m b
forever ma = ma >> forever ma

-- "fish"

infixr 1 >=>
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g

  --  do b <- f a
  --     g b

--------------------------------------------------------------------------------

-- Intuitívan, monád törvények:     (alapvető elvárás minden imperatív nyelvtől)

--   pure nem jár hatással
--      pure >=> f  =  f            do {x <- pure v; f x} = f v
--      f >=> pure  =  f            do {x <- f a; pure x} = f a

--   csak a műveletek *sorrendje* számít, a csoportosításuk nem (szekvencialitás)
--      minden imperatív program utasítások listája

--    f >=> (g >=> h)  = (f >=> g) >=> h
--    do {x <- f a; y <- g x; h y}   =   do {y <- do {x <- f a; g x}; h y}


-- State monad
--------------------------------------------------------------------------------


-- p :: State s a      -- művelet, ami egy darab "s" típusú változót tud
--                     -- mutable módon írni/olvasni

-- State s a --> tiszta nyelvbe beágyaz mutációt


-- intuíció: szimpla típusokkal hogyan modellezzük azt a függvényt, ami
-- "s" típusú állapotot módosít

--   s -> s        (nem elég, mert nincs visszatérési érték)
--     s             ->                  (a, s)
--   input állapot       (visszatérési érték, output állapot)


newtype State s a = State (s -> (a, s))

-- instance Functor (State s)
-- instance Applicative (State s)
-- instance Monad (State s)
-- get, put függvények (állapot-módosító függvények)
