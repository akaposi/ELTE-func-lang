{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- import Prelude hiding (Foldable(..))

-- Applicative (Prelude-ben)
--------------------------------------------------------------------------------

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b    -- kiejtése: "ap"

-- importálni: Control.Monad-ból:   ap :: Monad => m (a -> b) -> m a -> m b

-- mi az extra feature Functor-hoz képest?

-- fmap :: (a -> b) -> f a -> f b

-- 2-paraméteres fmap?
-- csak Functor instance-al ez nem definiálható.
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c

-- ha valami Monad, akkor fmap2 definiálható:
fmap2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
fmap2 f ma mb = do
  a <- ma
  b <- mb
  pure (f a b)

-- Applicative extra képesség Functor-hoz képest:
-- definiálható minden n-re: az n-paraméteres fmap

fmap2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2' f fa fb = fmap f fa <*> fb
   -- fa        :: f a
   -- f         :: a -> (b -> c)
   -- fmap f fa :: f (b -> c)
   -- fmap f fa <*> f b :: f c     OK

fmap2'' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fmap2'' f fa fb = (f <$> fa) <*> fb

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc = f <$> fa <*> fb <*> fc

fmap4 :: Applicative f => (a -> b -> c -> d -> e)
         -> f a -> f b -> f c -> f d -> f e
fmap4 f fa fb fc fd = (f <$> fa) <*> fb <*> fc <*> fd
   -- ap-ot azért ap-nak hívják, mert az a függvényalkalmazás
   -- f belsejében.


-- Miért akarjuk elkülöníteni az n-áris fmap-elést a Monad-tól?
-- Tárgy során nem fogunk tanulni olyan Applicative instance-ot, ahol ez
-- a különbség fontos.

-- példa:
-- függvény: (++) :: String -> String -> String
-- bináris fmap-el  : IO String -> IO String -> IO String
f1 :: IO String
f1 = (++) <$> getLine <*> getLine

-- Nem lehetséges a következő függvény csak Applicative-al:
f2 :: IO ()
f2 = do
  l1 <- getLine
  l2 <- getLine
  putStrLn (l1 ++ l2)

f3 :: IO ()
f3 = do
  l1 <- getLine
  case l1 of
    [] -> pure ()
    _  -> putStrLn "foo"

-- f2-f3 függvények jellemzője:
--  végrehajtunk először egy mellékhatásos műveletet,
--  és utána műveleteket *attól függően" hajtunk
--  végre műveleteket, hogy mi volt a visszatérési érték.

-- Applicative: futtatás előtt ismert az összes mellékhatás
-- Monad: dinamikus elágazás, hatások függenek az előző hatások értékeitől

-- (Applicative: van mellékhatás, de nincs interakció)
-- ipari példa: Facebook Haxl-nevű Haskell library-ja
--   spam-szűrésre használják
--   Applicative, amiben adatbázis-lekérdezést + expert system jellegű
--     lekérdezést lehet írni

--  mivel statikusan ismertek a hatások, ezért mielőtt elküldjük a tényleges
--  lekérdezéseket, lehet őket aggregálni, optimalizálni, stb.

--  (általánosan: lehet statikus elemzést csinálni)

-- ("Monadikus" adatbázis query: kérdezek, kapott adattól függően
--   kérdezhetek még 10-szer, vagy 0-szor)
-- (korlátozott statikus elemzés)

-- Haxl konkrétan:
--   - túl limitált, ha csak Applicative lekérdezést írhatunk
--   - lehet monadikus műveletet is
--   - GHC-nek van olyan feature-e, hogy akármilyen kifejezést írunk do notációval
--       vagy Monad instance-al, GHC megpróbálja minél inkább Applicative
--       műveletkre fordítani.
--   - (feature neve: "ApplicativeDo")


-- Jó feladatok: ha kapunk egy monadikus függvényt
--   kérdés 1: lehet-e ezt a függvényt Applicative-an definiálni?
--                 (interaktív-e ez a program valójában?)
--          2: adjuk meg az ekvivalens Applicative definíciót

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = pure []
mapM' f (a:as) = do
  b <- f a
  bs <- mapM' f as
  pure (b:bs)

-- nem interaktív, ezért át lehet írni Applicative-ra
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []                       -- pure: 0 aritású lift
mapA f (a:as) = (:) <$> f a <*> mapA f as     -- 2 aritású list (fmap)
-- Functor nem elég, Monad túl sok, Applicative OK
-- minél gyengébb a constraint, annál több esetben használható a függvény


-- példa függvény: elég Functor
-- "mellékhatásos" függvényt alkalmazunk egy tuple első mezőjére
mapFst :: Applicative f => (a -> f b) -> (a, c) -> f (b, c)
mapFst f (a, c) = (,) <$> f a <*> pure c
   -- (függvény "lift"-elése)
   -- (\fa fb -> (,) <$> fa <*> fb) :: f a -> f b -> f (a, b)

mapFst' :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
mapFst' f (a, c) = fmap (\b -> (b, c)) (f a)
  -- f a :: f b
  -- szeretnénk: f (b, c)
  -- ezt csak fmap használatával

-- általános kérdés: milyen Applicative művelet adhatú meg Functor műveletként:
--   ha csak 1 paraméteres map-elést használunk, azaz
--   minden n-paraméteres map-nél legfeljebb 1 paraméter nem pure.


-- Érdemes lehetőség szerint Applicative műveletet használni monadikus helyett
-- Control.Applicative modult érdemes megnézni
-- (<$), (<$>)
-- (túlzásokba ne essünk)
-- (hasonló, mint (.) és egyéb kombinátorok esete, ahol lehet túltömörített
--  kódot írni)


-- Foldable, Traversable
--------------------------------------------------------------------------------


{-
-- Foldable: osztály, aminek a foldr a metódusa
-- fold-olást általánosítom valamilyen container típusokra (lista, fa, pár, stb)
class Foldable f where
  foldr :: (a -> b -> b) -> b -> f a -> b

instance Foldable [] where
  foldr f z []     = z
  foldr f z (a:as) = f a (foldr f z as)

data Twice a = Twice a a

instance Foldable Twice where
  -- csaló verzió: foldr f z (Twice x y) = foldr f z [x, y]
  foldr f z (Twice x y) = f x (f y z)

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- házi feladat:
-- összes Leaf-ben levő értéket kombinálja
instance Foldable Tree where
  foldr = undefined
-}

-- példák:
data Tree a = Leaf a | Node a (Tree a) (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

-- sum (Node 0 (Leaf 10) (Leaf 20)) == 30
-- length (Node 30 (Leaf 10) (Leaf 20))
-- maximum (Node 30 (Leaf 10) (Leaf 20)) == 30

-- Data.Foldable: további Foldable függvényeket nézni
-- (minden függvény, ami foldr-el megírható, az minden Foldable-re működik)

-- Üres-e a struktúra?
null' :: Foldable t => t a -> Bool
null' ta = foldr (\_ _ -> False) True ta

-- (null függvény Tree-re fölösleges, mert nincsen üres Tree)


-- Traversable
--------------------------------------------------------------------------------

-- osztály, ahol túlterheljük a mapM függvényt

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]

-- class (Functor t, Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- mapM általánosítása két féle módon:
--   - Applicative megszorítás Monad helyett
--   - nem csak listán működik

-- példa:
-- traverse print (Node 10 (Leaf 20) (Leaf 30))

-- traverse
--    (\x -> if x < 10 then Nothing else Just x)
--    (Node 10 (Leaf 20) (Leaf 20)) == Just (Node 10 (Leaf 20) (Leaf 20))

-- traverse (\x -> if x < 10 then Nothing else Just x)
--    (Node 10 (Leaf 9) (Leaf 20)) == Nothing

traverseList :: Applicative f => (a -> f b) -> [a] -> f [b]
traverseList = mapA

traverseTree :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTree f (Leaf a)     = Leaf <$> f a    -- unáris lift (fmap)
traverseTree f (Node a l r) =
  Node <$> f a <*> traverseTree f l <*> traverseTree f r
    -- bináris lift

-- ajánlott megvizsgálni: mi történik, ha X adattípust, M monádban traverse-elünk

-- példa: fa bejárása lista monádban?
--        lásd előző előadás végét: Tree is monád
--        mi történik akkor, ha Tree bejárása Tree monádban?
--          (házi feladat)
