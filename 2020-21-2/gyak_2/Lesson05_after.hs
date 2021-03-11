{-# options_ghc -Wincomplete-patterns #-}
{-# language InstanceSigs #-}

import Control.Monad

{- composeEither variation -}

data Error = Error deriving (Show, Eq)

composeEither'' ::
  (b -> (Either Error) c) ->
  (a -> (Either Error) b) ->
  (a -> (Either Error) c)
-- composeEither'' f g a = either Left f (g a)
-- composeEither'' f g a = (g a) >>= f
-- composeEither'' = flip (>=>)
composeEither'' = (<=<)

{- Monad instance variation -}

data Result a = Ok a | Err String deriving (Show, Eq)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f ra = ra >>= (return . f)
  -- fmap f ra = ra >>= (\a -> return (f a))
  -- fmap f ra = do
    -- f <- rf
    -- a <- ra
    -- return (f a)

instance Applicative Result where
  pure :: a -> Result a
  pure = return

  (<*>) :: Result (a -> b) -> Result a -> Result b
  -- rf <*> ra = rf >>= (\f -> ra >>= (\a -> return (f a)))
  rf <*> ra = do
    f <- rf
    a <- ra
    return (f a)

instance Monad Result where
  return :: a -> Result a
  return = Ok

  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Ok a) >>= f = f a
  (Err e) >>= f = Err e


paros :: Int -> Bool
paratlan :: Int -> Bool

paros 0 = True
paros n = paratlan (n - 1)

paratlan 0 = False
paratlan n = paros (n - 1)


-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) ma mb = ma >>= \_ -> mb

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
-- (>=>) f g a = f a >>= g


{- IO -}

-- getLine :: IO String         -- read user input until a newline character
-- putStrLn :: String -> IO ()  -- output a string and end the line
--
-- print :: Show a => a -> IO ()
-- print = putStrLn . show

io :: IO ()
io = do
  name <- getLine
  print ("Szia, " ++ name ++ "!")

io' :: IO ()
io' = undefined


{- Standard library functions -}

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = pure []
sequence' (a:as) = do
  result <- a
  rest <- sequence' as
  pure (result : rest)

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' = (sequence .) . replicate

forever' :: Monad m => m a -> m b
forever' ma = ma >> forever' ma

smallEven :: Int -> Maybe Bool
smallEven x = if (x < 10) then Just (even x) else Nothing

-- filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- filterM' = undefined

-- mapM' :: Monad m => (a -> m b) -> [a] -> m [a]
-- mapM' = undefined

zipWithM' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' f [] _ = pure []
zipWithM' f _ [] = pure []
zipWithM' f (a:as) (b:bs) = do
  fab <- f a b
  rest <- zipWithM' f as bs
  pure (fab : rest)

-- foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
-- foldrM' = undefined


{- Practice -}

f1 :: Monad m => (a -> b) -> m a -> m b
f1 = undefined

f2 :: Monad m => m a -> m b -> m (a, b)
f2 = undefined

f3 :: Monad m => m (m a) -> m a
f3 = undefined

f4 :: Monad m => m (a -> b) -> m a -> m b
f4 = undefined

f5 :: Monad m => (a -> m b) -> m a -> m b
f5 = undefined

f6 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
f6 = undefined

f7 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
f7 = undefined

f8 :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f8 = undefined

-- Írj egy függvényt, ami beolvas egy sort stdin-ről, majd a sort kinyomtatja
-- annyiszor, ahány karakter van a sorban!
io1 :: IO ()
io1 = do
  line <- getLine
  replicateM_ (length line) (print line)
  -- pure ()

-- Írj egy függvényt, ami addig olvas be ismételten sorokat stdin-ről,
-- amíg a sor nem tartalmaz 'x' karaktert.
-- Ha a sorban 'x' van, akkor a program nyomtassa ki az összes
-- eddig beolvasott sort és térjen vissza.
io2 :: IO ()
io2 = undefined -- use `elem`
