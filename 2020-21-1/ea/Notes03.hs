
-- Maybe monád: mapMaybe
-- IO, main
-- do notáció
-- generikus Monad függvények; mapM

--------------------------------------------------------------------------------

-- Általánosan: osztály Monad
-- Functor f => Applicative f =>

-- 3 osztályban 4 darab metódus összesen
-- class Functor f where ...
-- class Functor f => Applicative f where ...
-- class Applicative f => Monad f where ...


-- Mellékhatásos programozás:
--   alapból minden tisztán funkcionális Haskell-ben
--     pl ha látunk (f :: Int -> Int), akkor tudjuk, hogy f nem csinál semmi rejtett hatást
--                                     (tesztelés, optimalizálás, refaktorálás szempontjából legjobb)
--     mutáció: sok algoritmus (gráf, fa manipulálások, bejárások, compiler pass-ok)
--              (lehet emulálni tiszta függvényekkel, de sokkal zajosabb lesz a kód)
--     kivételek: (szintén emulálható tisztán, de megint csak zajos lesz a kód az explicit hibakezelés miatt)
--        (Haskell-ben alapból nincs mutáció+exception, viszont, ha tudunk, ha szeretnénk, kiderül a típusból, hogy
--         egy függvény milyen mellékhatást használ)

--     "exotikus" mellékhatások: nem-determinisztikus / probabilisztikus programozás
--                               nem-det : értékadásnál lehet mondjuk (x legyen 0 vagy 1 vagy 2), a program többi része
--                                lefut mind a három értékre! (Prolog/Datalog stílusban programozni (vagy SQL))
--     ...


-- Maybe programozás
--------------------------------------------------------------------------------

-- instance Monad Maybe where ...

-- ha van egy függvény (a -> Maybe b), akkor az olyan függvény, ami hibázhat
-- map, foldr, filter, stb... nem használható változtatás nélkül

-- ha bárhol Nothing-ot ad a kapott függvény, akkor legyen végeredmény Nothing,
-- egyébként pedig legyen (Just <map-elt lista>)
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]   -- hibázni tudó (a -> b) függvényból kapunk hibázni tudó ([a] -> [b]) függvényt
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case mapMaybe f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

-- páros hosszú listák típusa
data List a = Nil | Cons a a (List a)

mapMaybe' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybe' f Nil            = Just Nil
mapMaybe' f (Cons a a' as) = case f a of
  Nothing -> Nothing                        -- hibakód vizsgálata mindenhol!
  Just b  -> case f a' of
    Nothing -> Nothing
    Just b' -> case mapMaybe' f as of
      Nothing -> Nothing
      Just bs -> Just (Cons b b' bs)

-- mi erre a megoldás? a hiba vizsgálat kifaktorálása

-- csak a belső "a"-ra vagyok kíváncsi (ha Nothing, akkor úgyis Nothing a végeredmény)
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  f = Nothing
bind (Just a) f = f a

mapMaybeB :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeB f []     = Just []
mapMaybeB f (a:as) =
  -- bind (f a) (\b -> bind (mapMaybeB f as) (\bs -> Just (b:bs)))

  bind (f a) $ \b ->                 -- kössük (f a) értékét b-hez
  bind (mapMaybeB f as) $ \bs ->     -- kössük (mapMaybeB f as) értékét bs-hez
  Just (b:bs)                        -- térjünk vissza (b:bs)-el

  -- imperatív nyelvben (a -> Maybe b) helyett, egyszerűen (a -> b) viszont mindig lehet hibát dobni
  -- pszeudokód (automatikus a kivétel propagálása)
  -- var b = f a;
  -- var bs = mapMaybeB f as;
  -- return (b:bs)

mapMaybeB' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybeB' f Nil            = Just Nil
mapMaybeB' f (Cons a a' as) =
  bind (f a) $ \b ->
  bind (f a') $ \b' ->
  bind (mapMaybeB' f as) $ \bs ->
  Just (Cons b b' bs)

{-
class Functor m => Monad m where
  -- bind függvény: operátor

  -- egymás után végezzük el két mellékhatásos műveletet
  -- második művelet függhet az első végeredményétől: (a -> m b) kódolja a függőséget
  (>>=) :: m a -> (a -> m b) -> m b

  -- (Maybe return-je: Just konsktruktor)
  -- (adjunk vissza egy tiszta "a" értéket, minden mellékhatás nélkül)
  return :: a -> m a

-- minden Monad instance egy custom mellékhatás implementáció
-- két metódus: 1. szekvenciálni tudjunk műveleteket (bind), egymás után végrehajtás
--              2. legyen lehetőség mellékhatás nélküli műveletre (return)
-}

-- instance Monad Maybe where
--   return = Just
--   (>>=)  = bind

mapMaybeB'' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybeB'' f Nil            = return Nil
mapMaybeB'' f (Cons a a' as) =
  f a >>= \b ->
  f a' >>= \b' ->
  mapMaybeB' f as >>= \bs ->
  return (Cons b b' bs)

-- szintaktikus cukor bind-ra (do notáció)
mapMaybeB''' :: (a -> Maybe b) -> List a -> Maybe (List b)
mapMaybeB''' f Nil            = return Nil
mapMaybeB''' f (Cons a a' as) = do
  b  <- f a       -- (nyíl kiejtése is "bind")
  b' <- f a'
  bs <- mapMaybeB''' f as
  return (Cons b b' bs)

-- alternatív (do után behúzva ugyanabban az oszlopban bind kifejezések
-- mapMaybeB''' f (Cons a a' as) =
--   do b  <- f a       -- (nyíl kiejtése is "bind")
--      b' <- f a'
--      bs <- mapMaybeB''' f as
--      return (Cons b b' bs)

-- mindig,
-- exp >>= \a ->   helyette   a <- exp
-- ...                        ...

-- konstans függvény bind-olás
-- (két mellékhatásos művelet egymás után végrehajtva)
--  második művelet nem függ az első eredményétől
bindConst :: Maybe a -> Maybe b -> Maybe b
bindConst ma mb = bind ma (\_ -> mb)

-- kiírva:
-- bindConst Nothing  _  = Nothing
-- bindConst (Just _) mb = mb

-- standard függvény:
-- (>>) :: Monad m => m a -> m b -> m b

-- Maybe példa  (>>)-vel

m1 :: Maybe Int
m1 = Just 0 >>
     Just 2 >>
     Just 10
     -- Just 10

m2 :: Maybe Int
m2 = Just 0 >>
     Nothing >>
     Just 10
     -- Nothing

-- lefordul az m1 kifejezésre
m1' :: Maybe Int
m1' = do
  Just 0
  Just 2
  Just 10

-- utolsó megjegyzés: lokális let-ek do blokkban

-- lefordul az m1 kifejezésre
m1'' :: Maybe Int
m1'' = do
  Just 0
  let foo = [0..10]    -- lehet let kifejezés is (nem kell "in" let után)
  Just 2
  let bar = True
  Just (sum foo)       -- (utolsó művelet nem lehet let kifejezés)


-- IO monád
--------------------------------------------------------------------------------

-- beépített típus (*nem* user definiálható!) (egyetlen példa, amit nézünk, ami nem user definiálható)
-- program input-output műveletek (stdin, stdout, terminál, grafika, hálózat, fájlrendszer, IPC)
-- Minden IO művelet mellékhatás, pl
--    "IO Int" típusú program: olyan program, ami elvégezhet tetszőleges IO műveletet, Int-el tér vissza.


-- main függvény:
-- () "unit típus": egy elemű típus, érték jelölése: ()
-- data One = One
-- data () = () (nem működik, mert () is szikatikus cukor, mint pl a pár típus)

-- (minden Haskell alkalmazásban egy db main függvény, ha fordítjuk a programot, akkor ez a függvény fut)
main :: IO ()
main = getLine >>= \line -> putStrLn (line ++ line)

main' = do
  line <- getLine
  putStrLn (line ++ line)

-- getLine :: IO String           beolvas egy sort a command line-ról
-- putStrLn :: String -> IO ()    kinyomtat egy String-et terminálra (+új sort)
-- ghci-ben minden IO a típusú érték, ha ghci-ben enter-t nyomunk rá, lefut
-- Prelude dokumentáció: megnézni az összes IO függvényt


-- "generikus" monadikus függvények
--------------------------------------------------------------------------------

-- mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
-- definíció nem használ semmit, ami a Maybe-re specifikus!
-- működik minden "m" monádra, nem csak a Maybe-re.

-- mapM standard függvény
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f []     = return []
mapM' f (a:as) = do
  b  <- f a
  bs <- mapM' f as
  return (b:bs)

-- példa:
-- mapM' (\n -> if even n then Nothing else Just (n + 10)) [1, 3, 5, 7] == Just [11,13,15,17]
-- mapM' (\n -> if even n then Nothing else Just (n + 10)) [1, 3, 5, 4, 7] == Nothing
-- mapM' (\n -> putStrLn (show n)) [0..10] -- számokat nyomtat ki

-- konvenció elnevezésre:
--     tiszta függvény:                                f
--     monadikus változat:                             fM
--     monadikus változat, viszont m () a végeredmény: fM_

-- standard: mapM_
-- olyan mapM, hogy nem hozza létre a map-elt listát
-- nincs map-elés, csak mellékhatások
mapM'_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM'_ f []     = return ()
mapM'_ f (a:as) = f a >> mapM'_ f as


-- standard: sequence
-- input: mellékhatásos műveletek listája
--        elvégezzük sorban az összes műveletet, a végeredményt visszaadjuk listában
sequence' :: Monad m => [m a] -> m [a]
sequence' = mapM' id
  -- mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
  -- mapM' id :: Monad m => [m a] -> m [a]

-- pl: sequence [putStrLn "foo", putStrLn "bar"]
--     sequence [Just 10, Just 20] == Just [10,20]
--     sequence [Just 10, Just 20, Nothing] == Nothing


--------------------------------------------------------------------------------

-- mellékhatás első osztályú feature:
--     - user tud mellékhatást definiálni
--     - típusok kontrollálják hogy milyen hatást alkalmazhatunk
--     - mellékhatásos művelet létrehozása elkülönül a végrehajtástól
--       (műveleteket kombinálni, tárolni struktúrában, tudom kontrollálni, hogy
--        mikor hajtódik végre egy hatás)
