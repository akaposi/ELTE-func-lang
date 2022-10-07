
{-# language InstanceSigs #-}
{-# options_ghc -Wincomplete-patterns #-}

--------------------------------------------------------------------------------
-- Következő feladat: fmap, viszont lehet rekurzív/több paraméteres típusra
--   (egymásba ágyazott típus nem lesz)
--------------------------------------------------------------------------------

data D a = D1 a a | D2 Int a deriving (Eq, Show)

instance Functor D where
  fmap :: (a -> b) -> D a -> D b
  fmap f (D1 a1 a2) = D1 (f a1) (f a2)
  fmap f (D2 n a)   = D2 n (f a)

--------------------------------------------------------------------------------

-- data Foo a = Foo (a -> a -> Int) (a -> Int)

data Tree1 i a = Leaf1 a | Node1 (i -> Tree1 i a)

t1 :: Tree1 Bool Int
t1 = Leaf1 100

-- (Bool -> a) ~ (a, a)

-- ha i = Bool, akkor bináris fa
t2 :: Tree1 Bool Int
t2 = Node1 (\b -> if b then Leaf1 10 else Leaf1 20)

-- ha i-nek N lehetséges értéke van, akkor
-- Node1, N-szeres ágazik el

t3 :: Tree1 Int Int
t3 = Node1 (\i -> Leaf1 (i + 10))

t4 :: Tree1 Int Int
t4 = Node1 (\i -> Node1 (\j -> Leaf1 (i + j)))

-- végtelenül elágazó fa
t5 :: Tree1 String Int
t5 = Node1 (\s -> Leaf1 (length s))

-- Node1
--     ""    ->  Leaf1 0
--     "a"   ->  Leaf1 1
--     "b"   ->  Leaf1 1
--     "c"   ->  Leaf1 1
--     ...   ->  ...

instance Functor (Tree1 i) where
  fmap :: (a -> b) -> Tree1 i a -> Tree1 i b
  fmap f (Leaf1 a) = Leaf1 (f a)
  fmap f (Node1 t) = Node1 (\i -> fmap f (t i))
       -- a map-elt fa i-edik részfája = a régi fa i-edik részfája map-elve

  -- fmap f (Node1 t) = Node1 (fmap f . t)


data Tree2 a = Node2 a [Tree2 a] deriving Show

t21 :: Tree2 Int
t21 = Node2 100 []         -- ha [], akkor levél a Node2 (0-szoros elágazás)

t22 :: Tree2 Int           -- hármas elágazás
t22 = Node2 100 [Node2 10 [], Node2 20 [], Node2 30 []]

-- példa: JSON: fa belsejében tömb, amiben további fák


instance Functor Tree2 where
  fmap :: (a -> b) -> Tree2 a -> Tree2 b
  fmap f (Node2 a ts) = Node2 (f a) (fmap (fmap f) ts)
    -- ts :: [Tree2 a]

    -- mivel a lista típus beágyazva szerepel, ezért
    -- a megfelelő listafüggvényeket kell használni
    -- a Tree2 függvények megadásánál

------------------------------------------------------------

data Tree3 a = Leaf3 a | Node3 (Maybe (Tree3 a)) (Tree3 a) deriving Show

-- általánosan: fmap minden konstruktort változatlul hagy "f"-ben

instance Functor Tree3 where
  fmap :: (a -> b) -> Tree3 a -> Tree3 b
  fmap f (Leaf3 a)    = Leaf3 (f a)
  fmap f (Node3 ml r) = Node3 (fmap (fmap f) ml) (fmap f r)
    -- fmap     (fmap f) ml
    --  ^         ^
    --  |        Tree3 fmap
    -- Maybe fmap

newtype Id a = Id a deriving Show

instance Functor Id where
  fmap :: (a -> b) -> Id a -> Id b
  fmap f (Id a) = Id (f a)

newtype Const a b = Const a deriving Show

  -- mindig az utolsó paraméter fölött fmap-elünk

instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap f (Const c) = Const c

-- data Foo a = Foo Int
-- fmap f (Foo n) = Foo n

  -- mindig az utolsó paraméter fölött fmap-elünk

data F a b c = F1 a b | F2 b c | F3 c deriving Show

instance Functor (F x y) where
  fmap :: (a -> b) -> F x y a -> F x y b
  fmap f (F1 x y) = F1 x y
  fmap f (F2 y a) = F2 y (f a)
  fmap f (F3 a)   = F3 (f a)


-- Maybe monád
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a) (Tree a) deriving (Eq, Show)


-- Írd meg a következő függvényt. A függvény úgy működik, mint a lista "filter",
-- viszont ha a kapott (a -> Maybe Bool) függvény valamely elemre Nothing-ot ad,
-- akkor Nothing legyen a végeredmény, egyébként Just <szűrt lista>
filterMaybe :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe f []     = Just []
filterMaybe f (a:as) = case f a of
  Nothing -> Nothing
  Just b  -> case filterMaybe f as of
    Nothing -> Nothing
    Just as -> if b then Just (a:as) else Just as

  -- Nothing -> Nothing
  -- Just b  -> case filterMaybe f as of
  --   Nothing -> Nothing
  --   Just as -> Just (if b then (a:as) else as)

-- Alkalmazz egy (a -> Maybe b) függvény egy Tree minden levelére, ha bármelyik
-- alkalmazás Nothing-ot ad, legyen az eredmény Nothing!
mapMaybeTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree = undefined

-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt a bemenő listák
-- elemeire! Ha bármelyik függvényalkalmazás Nothing, akkor a kimenet legyen
-- Nothing, egyébként Just <lista zippelés eredménye>.
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined


-- definiáld újra a függvényeket (>>=) függvény felhasználásával! Ne használj
-- mintaillesztést Maybe típusú értékekre!
filterMaybe2 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe2 = undefined

mapMaybeTree2 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree2 = undefined

zipWithMaybe2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe2 = undefined


-- definiáld újra a függvényeket do notáció használatával!
filterMaybe3 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterMaybe3 = undefined

mapMaybeTree3 :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
mapMaybeTree3 = undefined

zipWithMaybe3 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe3 = undefined


-- IO monád
--------------------------------------------------------------------------------

-- Használjuk a következő standard függvényeket, illetve a do notációt.

-- getLine  :: IO String             -- beolvas egy sort
-- print    :: Show a => a -> IO ()  -- kinyomtat egy értéket amire van Show instance
-- putStrLn :: String -> IO ()       -- String-et nyomtat ki

-- (>>=)  :: IO a -> (a -> IO b) -> IO b
-- (>>)   :: IO a -> IO b -> IO b
-- return :: a -> IO a
-- fmap   :: (a -> b) -> IO a -> IO b


-- Írj egy függvényt, ami beolvas egy sort, majd visszaadja a sorban az 'a'    és 'z'
-- közötti karakterek számát.
io1 :: IO ()
io1 = undefined


-- Írj egy függvényt, ami beolvas egy sort, majd a sort kinyomtatja annyiszor,
-- ahány karakter van a sorban!
io2 :: IO ()
io2 = undefined


-- Írj egy függvényt, ami addig olvas be ismételten sorokat, amíg a sor nem
-- tartalmaz 'x' karaktert. Ha a sorban 'x' van, akkor a program nyomtassa ki az
-- összes eddig beolvasott sort és térjen vissza.
io3 :: IO ()
io3 = undefined


-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a
-- sorban a kisbetűk számát.  A Ctrl-c-c -vel lehet megszakítani a futtatást
-- ghci-ben.
io4 :: IO ()
io4 = undefined
