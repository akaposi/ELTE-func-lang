
-- Előadás (monádok bevezetés)
------------------------------------------------------------

-- motiváló példa:

-- Maybe típussal szeretnénk programozni.
-- Nothing legyen a hiba lehetősége.
-- Ha bárhol Nothing-ot ad egy függvény, akkor szeretnénk, hogy
-- az egész programunk Nothing-ot adjon.

-- (catch is megadható, de most nem foglalkozunk ezzel)
-- csak Nothing propagálásával foglalkozunk

-- konkrét példa 1:

-- bármelyik alkalmazás Nothing, akkor a végeredmény Nothing
-- egyébként pedig Just <mappelt lista>
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case mapMaybe f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

-- még egy példa:
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

-- van egy fmap :: (a -> b) -> Tree a -> Tree b
-- ennek a Maybe-e verziója:
mapTreeMaybe :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTreeMaybe f (Leaf a) = fmap Leaf (f a)

  -- fmap Maybe-re ebben az esetben:
  -- fmap Leaf Nothing == Nothing
  -- fmap Leaf (Just b) == Just (Leaf b)

  -- Prelude-ben fmap operátoros formája: (<$>)
  --    Leaf <$> f a

  -- case f a of
  --   Nothing -> Nothing
  --   Just b  -> Just (Leaf b)

mapTreeMaybe f (Node t1 t2) = case mapTreeMaybe f t1 of
  Nothing -> Nothing
  Just t1' -> case mapTreeMaybe f t2 of
    Nothing -> Nothing
    Just t2' -> Just (Node t1' t2')

-- valami szebb megoldást szeretnénk a Nothing propagálásra
--   két darab alap függvény elég arra, hogy viszonylag szépen
--   propagáljuk a Nothing-ot

-- első függvény: Just
-- második függvény: Nothing propagálás, neve: bind

-- bind függvény Maybe esetén:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f  = Nothing
bind (Just a) f = f a

mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe' f []     = Just []
mapMaybe' f (a:as) =
  -- bind (f a) (\b ->
  --   bind (mapMaybe' f as) (\bs ->
  --     Just (b:bs)))

  bind (f a) $ \b ->
  bind (mapMaybe' f as) $ \bs ->
  Just (b:bs)

  -- bind (f a) (\b ->
  -- bind (mapMaybe' f as) (\bs ->
  -- Just (b:bs)))

mapTreeMaybe' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTreeMaybe' f (Leaf a)     = Leaf <$> f a
mapTreeMaybe' f (Node t1 t2) =
  bind (mapTreeMaybe' f t1) $ \t1' ->
  bind (mapTreeMaybe' f t2) $ \t2' ->
  Just (Node t1' t2')

----------------------------------------

-- bind: propagálja a hibát
-- Just: valami "hibamentes" értéket bevisz a Maybe típusba

-- mellékhatás propagálása:
--   mellékhatás propagálása működik több különböző típus esetén
--   Maybe: Nothing propagálás

-- class Monad m where
--   return :: a -> m a                  -- Just :: a -> Maybe a
--   (>>=)  :: m a -> (a -> m b) -> m b  -- bind :: ...

-- instance Monad Maybe where
--   return = Just
--   (>>=)  = bind

-- (következő 2-3 alkalom: további Monad instance-ok tárgyalása)


-- IO monád
------------------------------------------------------------

-- futtatható programban kell egy "main :: IO ()" definíció
-- main :: IO ()   -- () kiejtése "unit"  (hasonló, mint a "void" C/C++ esetén)
-- main = ...

-- instance Monad IO where
--   return = ...
--   (>>=)  = ...

-- primitív, beégetett definíció a Monad IO instance
-- Haskell nyelvben primitív az IO monád.

-- IO propagál bizonyos mellékhatásokat
--  input-output műveletek

-- Maybe: kivétel dobás a mellékhatás
-- IO: mellékhatás: IO műveletek végrehajtása

-- példa:

-- standard:   getLine :: IO String
--             (mellékhatásos művelet, ha végrehajtjuk, String-et ad)

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- egymás után végrehajt két műveletet, ahol a második művelet függhet
-- az első művelet végeredményétől

-- standard:  putStrLn :: String -> IO ()
--            (kinyomtat std out-ra egy String-et, új sorral együtt)

f1 :: IO ()
f1 = getLine      >>= \str ->
     putStrLn str >>= \_ ->
     putStrLn str

-- hogy tudjuk f1-et végrehajtani?

-- main :: IO ()
-- main = f1

-- lefordítjuk ghc-vel, pl "ghc Main.hs"

-- egyszerűbben: ghci-be bemásoljuk, enter
-- (ghci-ben IO speciálisan van kezelve)

-- Prelude-ben van egy csomó IO függvény

-- további példa: print
-- print :: Show a => a -> IO ()
--  print a = putStrLn (show a)

f2 :: IO ()
f2 = print 10 >>= (\_ ->
     print "foobar" >>= (\_ ->
     print [0..10]))

-- standard megoldás
-- (>>) :: Monad m => m a -> m b -> m b
-- ma >> mb = ma >>= (\_ -> mb)

f2' :: IO ()
f2' = print 10 >> print "foobar" >> print [0..10]

-- Prelude-beli IO műveleteket meg lehet nézni

-- térjünk vissza a Maybe-re

m1 :: Maybe Int
m1 = Just 10 >> Just 100   -- Just 100

m2 :: Maybe Int
m2 = Nothing >> Just 100   -- m2 == Nothing

m3 :: Maybe Int
m3 = Just 10 >> Nothing

-- 2 darab példa Monad instance-ra
--   Maybe: mellékhatás: Nothing mint exception
--   IO   : mellékhatás: IO műveletek


-- data Either a b = Left a | Right b
-- instance Monad (Either a) where
--   return a = Right a
--   Left a  >>= f = Left a
--   Right b >>= f = f b

-- Either monád: hasonló, mint a Maybe, viszont
--  tudunk valamilyen "a" típusú adatot csatolni
--  egy kivételhez

g1 :: Int -> Int -> Either String Int
g1 n1 n2 | n1 >= 0 && n2 >= 0 = return (n1 + n2)
         | otherwise          = Left "valamelyik szám negatív"

-- mapListEither :: (a -> Either e b) -> [a] -> Either e [b]
-- (házi feladat, ugyanúgy megírható, mint a mapMaybe)

-- általános map egy monádban
mapMonad :: Monad m => (a -> m b) -> [a] -> m [b]
mapMonad f []     = return []    -- Maybe esetén: Just, Either esetén: Right
mapMonad f (a:as) =
  f a >>= \b ->
  mapMonad f as >>= \bs ->
  return (b:bs)

-- példa:
-- mapMonad (\x -> print x) [0..10]
--   print :: Show a => a -> IO ()

mapMonad_ :: Monad m => (a -> m b) -> [a] -> m ()
mapMonad_ f []     = return ()
mapMonad_ f (a:as) = f a >> mapMonad_ f as

-- mapMonad és mapMonad_: standard függvények:
--   mapM, mapM_  névvel

-- későbbiekben további Monad instance-ok:
--   State:  mellékhatás: van egy mutábilis írható/olvasható változó
--                        referencia változása propagálódik

--   Reader:  mellékhatás: van egy csak olvasható referencia, ami
--            lekérdezhető bárhol a Reader monádban
--            (nem kell valamilyen program konfigot odaadni extra argumentumként
--             minden egyes függvénynek)

--   [] (lista típuskonstruktor) : TODO

-- imperatív nyelvekben: mindig egy default monádban dolgozunk, ahol
--   mindig elérhető IO, exception, mutation

-- Haskell-ben: alapból nincsen semmi mellékhatás
--   de ha mégis szeretnénk mellékhatást:
--      akkor használunk valamilyen monádot
--      kiderül a típusokból, hogy milyen mellékhatás lehetséges
