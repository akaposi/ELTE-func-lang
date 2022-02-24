{-# language InstanceSigs #-}

--------------------------------------------------------------------------------

{-
Funktor törvények (laws)
  példa: Eq: (==) szimmetrikus, tranzitív, reflexív

class Functor f where
  fmap :: (a -> b) -> f a -> f b

  - 1. fmap id x = x
        -- struktúra nem változhat, csak a belső "a" értékek.
        --   ellenpélda: myMap f xs = reverse (map f xs)
        --   (fmap definíció: minden konstruktort ugyanarra a konstruktorra képezünk)

  - 2. fmap f (fmap g x) = fmap (f . g) x
        -- "optimalizáció" : két bejárás helyett elég egy
        -- map f (map g xs) két listát hoz létre, map (f.g) xs csak egyet

        -- GHC nem használja ezt a tulajdonságot.
         -- GHC nem tudja betartani a Functor törvényeket a programozókkal

        -- Viszont: REWRITE pragma: custom optimalizációs átírásokat lehet GHC-hez adni
        -- {-# REWRITE fmapRule2 :: fmap f (fmap g x) = fmap (f . g) x #-}
-}

-- newtype: csak 1 konstruktor, 1 mezővel
--          nincs futásidejű költség

newtype Fun a b = Fun (a -> b)   -- wrapper a függvényeken

instance Functor (Fun c) where
  fmap :: (a -> b) -> Fun c a -> Fun c b
  fmap f (Fun g) = Fun (\c -> f (g c))
      -- új sorozat c-edik eleme = f (sorozat c-edik eleme)

-- (a -> b)  :    (b_0, b_1, b_2, ....   )
--                  minden a értékhez egy b érték
-- fmap f után     (f b_0, f b_1, f b_2, ....   )

f1 :: (->) Int Int
f1 = (+20)

-- instance Functor ((->) c) where
--   fmap :: (a -> b) -> (c -> a) -> (c -> b)
--   fmap = (.)

data ITree i a = ILeaf a | INode (i -> ITree i a)

-- Tree Bool a    :  bináris fa

t1 :: ITree Bool Int
t1 = INode (\b -> if b then ILeaf 0 else ILeaf 1)

t2 :: ITree Int Int
t2 = INode (\n -> ILeaf (n + 10))

t3 :: ITree Int Int
t3 = INode (\n -> INode (\m -> ILeaf (n + m)))

instance Functor (ITree i) where
  fmap f (ILeaf a)  = ILeaf (f a)
  fmap f (INode ts) = INode (fmap f . ts)
                -- = Node (\i -> fmap f (ts i))

-- Monad
--------------------------------------------------------------------------------

-- (custom mellékhatáshoz interface-t biztsít ez az osztály)

-- class Monad where


-- konkrét eset (instance): Maybe
--    Nothing : hiba
--    Just a  : nincs hiba

--    Either e a
--      Left e  : hiba, további info "e"
--      Right a : nincs hiba


-- hibakód kezelés problémái:

f :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
f (Just n1) (Just n2) (Just n3) = Just (n1 + n2 + n3)
f _ _ _ = Nothing

-- map-elek egy listát, (a -> Maybe b)

-- ha bárhol Nothing-ok kapok, akkor a végeredmény is Nothing
mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = Just []
mapMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case mapMaybe f as of
    Nothing -> Nothing
    Just bs -> Just (b:bs)

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a)

mapTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree f (Leaf a) = case f a of
  Nothing -> Nothing
  Just b  -> Just (Leaf b)
mapTree f (Node t1 t2 t3) = case mapTree f t1 of
  Nothing -> Nothing
  Just t1' -> case mapTree f t2 of
    Nothing -> Nothing
    Just t2' -> case mapTree f t3 of
      Nothing -> Nothing
      Just t3' -> Just (Node t1' t2' t3')

mapTree' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree' f (Leaf a) = fmap Leaf (f a)
mapTree' f (Node t1 t2 t3) = case mapTree' f t1 of
  Nothing -> Nothing
  Just t1' -> case mapTree' f t2 of
    Nothing -> Nothing
    Just t2' -> fmap (\t3' -> Node t1' t2' t3') (mapTree' f t3)

-- faktoráljuk ki a hibakezelést egy függvénybe

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  f = Nothing   -- hibaeset
bind (Just a) f = f a       -- sikeres eset


-- mapTree'' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
-- mapTree'' f (Leaf a)        = fmap Leaf (f a)
-- mapTree'' f (Node t1 t2 t3) =
--   bind (mapTree'' f t1) (\t1' ->
--   bind (mapTree'' f t2) (\t2' ->
--   bind (mapTree'' f t3) (\t3' ->
--   Just (Node t1' t2' t3'))))

mapTree'' :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree'' f (Leaf a)        = fmap Leaf (f a)
mapTree'' f (Node t1 t2 t3) =
  bind (mapTree'' f t1) $ \t1' ->
  bind (mapTree'' f t2) $ \t2' ->
  bind (mapTree'' f t3) $ \t3' ->
  Just (Node t1' t2' t3')

  -- var t1' = mapTree''(f, t1) ;
  -- var t2' = mapTree''(f, t2) ;
  -- var t3' = mapTree''(f, t3) ;
  -- return Node(t1', t2', t3') ;


  -- bind ~ ";"    (szekvenciálás: mellékhatásos műveletek egymás után végrehatjása)

  -- Maybe : mellékhatás : hibalehetőség (kivétel dobás extra info nélkül)
  --         bind        : egymás utáni mellékhatásos végrehajtás


-- class Functor m => Monad m where
--   return :: a -> m a                     -- mellékhatás *nélkül* becsomagolja/visszaadja az "a" értéket
--   (>>=)  :: m a -> (a -> m b) -> mb      -- "bind"

-- instance Monad Maybe where
--   return :: a -> Maybe a
--   return a = Just a

--   (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--   (>>=) = bind

mapTree0 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree0 f (Leaf a) =
  f a >>= \b -> return (Leaf b)
mapTree0 f (Node t1 t2 t3) =
  mapTree0 f t1 >>= \t1' ->
  mapTree0 f t2 >>= \t2' ->
  mapTree0 f t3 >>= \t3' ->
  return (Node t1' t2' t3')


-- cukor a Monad osztályhoz: "do notáció"

mapTree1 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapTree1 f (Leaf a) = do
  b <- f a
  return (Leaf b)
mapTree1 f (Node t1 t2 t3) = do
  t1' <- mapTree1 f t1             -- "t1'-t bind-olom a mapTree1 f t1-ből"
  t2' <- mapTree1 f t2
  t3' <- mapTree1 f t3
  return (Node t1' t2' t3')

-- mapTree1 f (Node t1 t2 t3) =
--   do t1' <- mapTree1 f t1
--      t2' <- mapTree1 f t2
--      t3' <- mapTree1 f t3
--      return (Node t1' t2' t3')

-- ma >> mb    :   egymás után hajtsuk végre ma-t majd mb-t
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb

m1 :: Maybe Int
m1 = do
  Just 0
  Nothing
  Just 2
  Just 3

-- m1 :: Maybe Int
-- m1 = do
--   var a = {return 0;};
--   var b = throw();
--   var c = {return 2;};
--   var d = {return 3;};
--   ...


-- do notáció fordítása:

--   a <- ma     --->    ma >>= \a ->
--   p                   p


--   ma          --->    ma >>
--   p                   p



-- Mese
------------------------------------------------------------

-- class Functor f where
--   fmap

-- class Functor f => Applicative f where
--   (<*>)

-- class Applicative m => Monad m where
--   return
--   (>>)

-- 3 class, 4 metódus

-- Functor --> Applicative --> Monad   :   matematikából jön  (kategóriaelmélet)
--                                         kategóriaelmélet: "kód duplikáció" elkerülése matematikában


-- további Maybe függvények
------------------------------------------------------------

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f (a:as) (b:bs) = do
  c <- f a b
  cs <- zipWithM f as bs
  return (c:cs)
zipWithM f _ _ = return []

-- zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
-- zipWithMaybe f (a:as) (b:bs) =
--   f a b >>= \c ->
--   zipWithMaybe f as bs >>= \cs ->
--   return (c:cs)
-- zipWithMaybe f _ _ = return []

-- mapMaybe1 :: (a -> Maybe b) -> [a] -> Maybe [b]

-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM f []     = return []
-- mapM f (a:as) = do
--   b <- f a
--   bs <- mapM f as
--   return (b:bs)

-- standard elnevezés:
--   tiszta függvény:                                f
--   mellékhatásos verzió:                           fM
--   mellékhatásos verzió, viszont végeredmény ():   fM_

-- zipWith   ::            (a -> b -> c  ) -> [a] -> [b] ->   [c]
-- zipWithM  :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
-- zipWithM_ :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()

-- Általánosan:
--   ha (Monad m)  :
--      m a        : mellékhatásos művelet, "m" megadja a hatást, "a" a visszatérési érték típusa

-- pl: Maybe Int   : hatás: Maybe (hibadobás), visszatérési érték típusa: Int


-- IO példát
------------------------------------------------------------

-- instance Monad IO    -- (built-in típus)
-- IO a    :   művelet, ami IO mellékhatást csinálhat, "a" értékkel tér vissza


-- putStrLn :: String -> IO ()

p1 :: IO ()
p1 = do
  putStrLn "hello world"
  putStrLn "hello world"
  putStrLn "hello world"


-- ghci-ben : IO a típusú értékek ütök be, akkor az végrehajtódik
-- másik mód: main függvény, le lehet fordítani futtatható állományra
--     (10-20x gyorsabb lefordítva)

main :: IO ()
main = p1


-- getLine :: IO String      -- egy sort beolvas stdin-ról
p2 :: IO ()
p2 = do
  l <- getLine
  putStrLn $ l ++ l


--------------------------------------------------------------------------------
