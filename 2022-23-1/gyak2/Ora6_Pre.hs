{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE DeriveFunctor, InstanceSigs #-}
module Ora6 where


import Control.Applicative
import Control.Monad

-- IO emlékeztető
{-
getLine :: IO String       -- beolvas egy string standard inputról
putStr  :: String -> IO () -- kiír egy stringet standard outputra, eredmény nincs ezért IO ()-
-}

-- IO műveletek szekvenciálása
io1 :: IO ()
io1 = putStr "Hello World\n" >> putStr "Hello World, again!\n"
-- Applikatív szekvenciálás is működik
-- io1 = putStr "Hello World\n" *> putStr "Hello World, again!\n"
-- Imperatív 'do' notáció
-- io1 = do
--      putStr "Hello World\n"
--      putStr "Hello World, again!\n"

-- Művelet eredményének felhasználása
io2 :: IO ()
io2 = getLine >>= \s -> putStr $ "Hello" ++ s
-- a getLine eredményét használjuk fel
-- >>= hasonló a pipehoz bashben
-- head -1 - | cat

-- do notációval:
{-
do
    s <- getLine
    putStr $ "Hello" ++ s
-}

-- Egyéb műveletek:
-- print :: Show a => a -> IO ()
-- print = putStrLn . show
-- readLn :: Read a => IO a
-- readLn = read <$> getLine
-- readLn nem pont így van implementálva, de az most nem releváns

readAndPrintInteger :: IO ()
readAndPrintInteger = do
    (i :: Integer) <- readLn
    print i

-- vagy

readAndPrintInteger' :: IO ()
readAndPrintInteger' = do
    i <- readLn :: IO Integer
    print i

-- vagy

-- Ugye lehet a readLn hibás, akkor nincs Integer érték ezért a print nem fog lefutni a >>= miatt
readAndPrintInteger'' :: IO ()
readAndPrintInteger'' = (readLn :: IO Integer) >>= print


-- Olvassunk be stdin-ről egy stringet és írjuk ki stdout-ra
getAndPrint :: IO ()
getAndPrint = undefined

-- Olvassunk be stdin-ről két számot és adjuk őket össze, majd írjuk ki stdoutra
addTwoNumbers :: IO ()
addTwoNumbers = undefined

-- olvassunk be n számot stdinről majd adjuk őket össze
-- használjunk pure/return-t
-- emlékeztető: replicateM
sumIO :: Int -> IO Int
sumIO = undefined

-- Mi az amit nem tud az IO?
-- Például nem tudunk állapotokat tárolni
-- Írjunk rá saját típust

-- newtype ugyanaz mint data, viszont csak akkor használható ha 1 db konstruktor van 1 db paraméterrel
newtype State s a = State (s -> (s,a))
-- Konstruktor 1 db s -> (a,s) típusú paraméterrel
-- s az állapot
-- a a mellékhatás
-- A data NEM egy állapot hanem egy tetszőleges állapotváltozás!!!!

-- segédfv
runState :: State s a -> (s -> (s, a))
runState (State f) s = f s


-- Ezt a runState és data konstruktort össze lehet vonni egy úgynevezett "rekord típusban"
-- data State s a = State { runState :: s -> (s,a) }
-- elnevezzük a paramétert runState-nek

-- Olyan effektus ami mellékhatásba visszaadja az állapotot
get :: State s s
get = State $ \s -> (s,s)

-- Olyan effektus amiben csak az állapotot írjuk felül (nincs mellékhatás)
put :: s -> State s ()
put s = State $ const (s, ())


-- instanceok írása
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    -- fmap :: (a -> b) -> (s -> (s,a)) -> (s -> (s,b))
    fmap f (State sf) = undefined

instance Applicative (State s) where
    pure :: a -> State s a
    -- pure :: a -> (s -> (s,a))
    pure a = undefined
    (<*>) :: State s (a -> b) -> State s a -> State s b
    -- (<*>) :: (s -> (s, a -> b)) -> (s -> (s, a)) -> (s -> (s, b))
    (<*>) = undefined
    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    -- liftA2 :: (a -> b -> c) -> (s -> (s, a)) -> (s -> (s, b)) -> (s -> (s, c))
    liftA2 = undefined

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    -- (>>=) :: (s -> (s,a)) -> (a -> (s -> (s, b))) -> (s -> (s,b))
    (>>=) = undefined


-- Állapot változás
modify :: (s -> s) -> State s ()
modify f = get >>= \a -> put (f a)

modify' :: (s -> s) -> State s ()
modify' f = do
    a <- get
    put (f a) -- nem returnt/puret használunk mert az a mellékhatásba rak valamit

-- Feladatok State-el

-- nézzük meg az állapotot és döntsük el, hogy nagyobb-e mint 20, ha igen állítsuk be 20-ra
-- a mellékhatás az legyen, hogy az eredeti állapot osztható-e 9-el
stateExample1 :: State Int Bool
stateExample1 = do
    st <- get -- eredeti állapot
    if st > 20 then put 20 else pure ()
    -- Egyágú if - csak akkor fut le az állapotváltozás ha a feltétel igaz
    -- when (st > 20) $ put 20
    pure (mod st 9 == 0)

{-
when :: Applicative f => Bool -> f a -> f ()
when True f = void f
when False _ = pure ()
-}

-- do nélkül: get >>= \st -> when (st > 20) (put 20) *> pure (mod st 9)
-- vagy     : get >>= \st -> mod st 9 <$ (when (st > 20) $ put 20)

-- nézzük meg az állapotot és döntsük el, hogy egyenlő e 10-el, ha igen a mellékhatás a függvény végén majd ez legyen.
-- A függvényben végezzünk el egy állapotváltozást ami négyzetre emeli a számot
stateExample2 :: State Int Bool
stateExample2 = undefined

-- emeljük az állapotot négyzetre 17x
-- mellékhatás nincs
-- emlékeztető: replicateM_
stateExample3 :: State Int ()
stateExample3 = undefined

-- A kapott paraméter alapján emeljük négyzetre az állapotot n-szer
-- Mellékhatásként adjuk vissza az összes köztes értéket
stateExample4 :: Int -> State Int [Int]
stateExample4 = undefined


data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving (Eq, Show)

-- címkézzük meg a fát egyre növekvő számokkal
-- használjunk state-et!
-- állapot legyen a számláló
-- mellékhatás legyen egy megcímzett fa
labelTree :: Tree a -> Tree (a, Int)
labelTree tr = snd (runState (go tr) 0)
    where
        go (Leaf a) = undefined
        go (Branch l a r) = undefined


-- Gyakorló feladatok:

-- A kapott paramétert rakjuk bele az állapotba, a régit adjuk vissza mellékhatásként
stateGyak1 :: a -> State a a
stateGyak1 = undefined

-- kapunk egy State-t paraméterül. A State "lefuttatása" után állítsuk vissza az eredeti állapotot
stateGyak2 :: State a b -> State a b
stateGyak2 = undefined

-- Szorozzuk meg a jelen állapotot 4-el
-- Ha az eredeti és az új állapot különbsége nagyobb mint 10 akkor a mellékhatás 3, egyébként 15 legyen
stateGyak3 :: State Int Int
stateGyak3 = undefined

-- címkézzünk meg egy rózsafát!
data RoseTree a = RoseLeaf a | RoseBranch [RoseTree a] deriving (Eq, Show)

labelRoseTree :: RoseTree a -> RoseTree (a, Int)
labelRoseTree = undefined


-- csináljuk meg ugyanezt csak egy másik bejárással!
labelTree2 :: Tree a -> Tree (a, Int)
labelTree2 = undefined

