
-- Functor
--------------------------------------------------------------------------------

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b   -- "map" függvény túlterhelése

  -- törvények:
  -- 1.   fmap id x = x
  -- 2.   fmap f (fmap g x) = fmap (f . g) x

-- instance Functor [] where
--    fmap = map

-- instance Functor Maybe where ...

-- Minden Functor, amiből "a" típusú értékeket (0 vagy több) ki lehet nyerni

-- Maybe a  : 0 vagy 1 "a" érték
-- [a]      : 0 vagy több (akár végtelen) "a" érték
-- (b -> a) : |b| darab "a" érték

-- függvény "a"-ba megy:

-- szintakikus cukorkák típusokra
--   lista: [a]       :: *   prefix: [] :: * -> *      (pl: [] Int)
--   pár:   (a, b)    :: *   prefix: (,) :: * -> * -> *  ((,) Int Bool :: *)
--   unit:  ()        :: *
--   függvény: a -> b :: *   prefix: (->) :: * -> * -> *   ((->) Int Int)


-- standard instance
--                (->) c :: * -> *     (input típus már meg van adva)
-- instance Functor ((->) c) where
--   -- fmap :: (a -> b) -> ((->) c a) -> ((->) c b)
--   -- fmap :: (a -> b) -> (c -> a) -> (c -> b)     -- kompozíció típusa!
--   fmap f g = f . g


-- Mi *nem* funktor?
--   f :: * -> *    hogy nincs Functor f instance?


-- nincs Functor instance, ha "a" egy függvény bemeneteként jelenik meg!
newtype F a = F (a -> Bool)

-- instance Functor F where
--   fmap f (F g) = F _
     -- _ :: F b
     -- F _      _ :: b -> Bool
     --          g :: a -> Bool
     --          f :: a -> b    kéne: b -> a !


-- (kitenkintés)
-- Functor: "kovariáns"

-- "kontravariáns" funktorok osztályaf
--    (kb. ugyanaz a variancia mint OOP kontextusban)
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant F where
  contramap f (F g) = F (g . f)



-- fa ADT példa (bináris leveles fa):
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- fmap (+10) (Node (Leaf 0) (Leaf 1)) ==
-- Node (Leaf 10) (Leaf 11)

-- végtelenül ágazó fa példa:

-- egyik lehetőség:
-- data ITree a = Leaf a | Node [ITree a]

-- "i": részfák indexei
data ITree i a = ILeaf a | INode (i -> ITree i a)

-- példa:

     -- INode minden Int értékhez egy részfát tartalmaz
     -- (nem végtelen elágazás: 2^64 lehetséges Int érték)
     -- pl: végtelen típus: String,   már [()] is végtelen
it1 :: ITree Int Bool
it1 = ILeaf True

it2 :: ITree Int Bool
it2 = INode $ \i -> if i < 10 then ILeaf True else ILeaf False

it3 :: ITree Int Int
it3 = INode $ \i -> INode $ \j -> ILeaf (i + j)
 -- 3 mélységű, 2^64 elágazású fa

 -- "beágyazott nyelvek"-ben gyakran használt technika


-- class Monad
------------------------------------------------------------

-- három osztály az anyag jelentős része:
-- Functor => Applicative => Monad

-- historikusan: először Functor (30-as évek matematika)
--                       Monad   (90-es évek)
--                   Applicative (2000-es évek)

-- 1. motiváció : szeretnénk Maybe-vel programozni
--    viszont nem szeretnénk folyton Nothing esetet vizsgálni!
--    C nyelv: hibakódok + esetszétválasztás mindenhol
--    máshol : exception: *csak* dobás és elkapás helyén foglalkozunk a
--             hibával

-- instance Monad Maybe : Maybe-t exception-ként használjuk
--    (mindig van választásunk a két stílus között)

f :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f ma mb mc = case ma of
  Nothing -> Nothing
  Just a  -> case mb of
    Nothing -> Nothing
    Just b  -> case mc of
      Nothing -> Nothing
      Just c -> Just (a, b, c)

-- konkrétan itt: a boilerplate redukálható
f' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f' (Just a) (Just b) (Just c) = Just (a, b, c)
f' _ _ _ = Nothing

-- lista összes elemét validálom
-- művelet sikeres (Just-ot ad), ha minden elemre sikeres
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case mapMaybe f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

-- mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
-- mapMaybe' f []     = Just []
-- mapMaybe' f (a:as) = case (f a, mapMaybe' f as) of
--   (Just b, Just bs) -> Just (b:bs)
--   _                 -> Nothing

-- absztraháljuk: Nothing mindig "rövidre zárja" a programot,
--                köztes eredmény Nothing: végső eredmény is Nothing

   --  köztes érték    hogyan dolgozzunk tovább    végső eredmény
bind :: Maybe a    ->   (a -> Maybe b)             -> Maybe b
bind Nothing  f = Nothing
bind (Just a) f = f a

-- feladat: mapMaybe függvény bind-al, nincs Maybe mintaillesztés

mapMaybe' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe' f []     = Just []
mapMaybe' f (a:as) =
  bind (f a) $ \b ->
  bind (mapMaybe' f as) $ \bs ->
  Just (b:bs)

  -- bind (f a) (\b -> bind (mapMaybe' f as) (\bs -> Just (b:bs)))

  -- imperatív program :

  -- let b = f(a);
  -- let bs = mapMaybe'(f, as);
  -- return (b:bs)

-- class Monad m where
--   (>>=)  :: m a -> (a -> m b) -> m b    -- megfelel a "bind" függvénynek
--   return :: m a                         -- megfelel a "Just"-nak


mapMaybe'' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe'' f []     = Just []
mapMaybe'' f (a:as) =
  f a >>= \b ->
  mapMaybe'' f as >>= \bs ->
  return (b:bs)

-- szintaktikus cukorka a >>=-ra: "do notáció"
-- imperatív kinézetű kód:
mapMaybe''' :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe''' f []     = return []
mapMaybe''' f (a:as) = do
  b <- f a
  bs <- mapMaybe''' f as
  return (b:bs)

-- fordítás  :

--  a <- ma            -->       bind ma $ \a ->
--  ...                          ...

-- ma                  --        bind ma $ \_ ->
-- ...                           ...


f'' :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
f'' ma mb mc = do
  a <- ma
  b <- mb
  c <- mc
  return (a, b, c)

  -- bind ma $ \a ->
  -- bind mb $ \b ->
  -- bind mc $ \c ->
  -- Just (a, b, c)

  -- case ma of
  -- Nothing -> Nothing
  -- Just a  -> case mb of
  --   Nothing -> Nothing
  --   Just b  -> case mc of
  --     Nothing -> Nothing
  --     Just c -> Just (a, b, c)

-- általánosan mik a Monad instance-ok?
--   minden instance egy custom mellékhatást definiál

-- (std instance)
-- instance Monad Maybe where
--   (>>=) = bind
--   return = Just

-- Maybe: egyszerű kivétel dobás, mint mellékhatás

-- data Either a b = Left a | Right b

-- instance Monad (Either a) : strukturált kivétel dobás

-- instance Monad (Either e) where
--   Left e  >>= f = Left e
--   Right a >>= f = f a
--   return = Right             -- return: mellékhatás nélkül visszaaadunk
--                              -- egy értéket

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f [] = return []
mapEither f (a:as) = do
  b <- f a
  bs <- mapEither f as
  return (b:bs)

  -- mapEither (\x -> if x < 10 then return x else Left "error") [0..11]
  -- == Left "error"

-- működik bármilyen user által definiált mellékhatásra!
-- mellékhatásos függvény: "monadikus" függvény (monadic)
--   standard függvények: tiszta verzió + monadikus verzió
mapMonad :: Monad m => (a -> m b) -> [a] -> m [b]
mapMonad f [] = return []
mapMonad f (a:as) = do
  b <- f a
  bs <- mapMonad f as
  return (b:bs)

-- IO monád
-- példa:
--  putStrLn :: String -> IO ()
--  IO a  -->  egy olyan program, ami I/O mellékhatást végrehajthat,
--             és futás végén egy "a" értéket ad

-- getLine :: IO String

myFun :: IO ()
myFun = do
  l <- getLine
  putStrLn (l ++ l ++ l)

-- teljes bináris fa definíciója:

data FullTree a = FLeaf a | FNode (FullTree (a, a))
-- "nested ADT"
-- invariáns lehet kódolni

ft1 :: FullTree Int
ft1 = FLeaf 10

ft2 = FNode (FLeaf (10, 20))
ft3 = FNode (FNode (FLeaf ((10, 20), (10, 20))))
ft4 = FNode (FNode (FNode (FLeaf (((10, 20), (10, 20)), ((10, 20), (10, 20))))))
