import Control.Applicative

-- Parser

newtype Parser a =
  Parser { run :: String -> Maybe (a, String) }

-- run (Parser par) = par

{-
aBetu :: String -> Bool
aBetu "a" = True
aBetu _   = False
-}

aBetu :: Parser ()
aBetu = Parser $ \s -> case s of
  'a':s -> Just (() , s)
  _     -> Nothing

instance Functor Parser where
   fmap :: (a -> b) -> Parser a -> Parser b
   fmap f par = Parser $ \s -> case run par s of
     Nothing     -> Nothing
     Just (a,s') -> Just (f a,s')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  para >>= aparb = Parser $ \ s -> case run para s of
    Nothing     -> Nothing
    Just (a,s') -> run (aparb a) s'
-- (<*>) a monadbol
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf >>= \ f -> ma >>= (return . f)
{-
ap mf ma = mf >>= \ f -> ma >>= \ a -> return (f a)

ap mf ma = do
  f <- mf
  a <- ma
  return (f a)
-}

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a,s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  {-
  parab <*> para = Parser $ \s -> case run parab s of
    Nothing -> Nothing
    Just (ab,s') -> case run para s' of
      Nothing -> Nothing
      Just (a,s'') -> Just (ab a,s'')
  -}
  -- parab <*> para = parab >>= \ f -> para >>= (return . f)
  {-
  parab <*> para = Parser $ \ s -> case run parab s of
    Nothing -> Nothing
    Just (ab,s') -> run (para >>= (return . ab)) s'
  -}
  parab <*> para = Parser $ \ s -> case run parab s of
   Nothing -> Nothing
   Just (ab,s') -> case run para s' of
    Nothing     -> Nothing
    Just (a,s'') -> Just (ab a,s'')
  
par3a' :: Parser ()
par3a' = do
  aBetu
  aBetu
  aBetu

par3a :: Parser ()
par3a = aBetu *> aBetu *> aBetu

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((),"")
  _  -> Nothing

parCsak3a :: Parser ()
parCsak3a = aBetu *> aBetu *> aBetu *> eof

char :: Char -> Parser ()
char c = Parser $ \s -> case s of
  c':s' | c == c' -> Just ((),s')
  _               -> Nothing

string :: String -> Parser ()
string s = Parser $ \s' -> let (s1,s2) = splitAt (length s) s' in
  if s == s1 then
    Just ((),s2)
  else
    Nothing

-- regexp-ekben *
many' :: Parser a -> Parser ()
many' par = Parser $ \s -> case run par s of
  Nothing     -> Just ((),s)
  Just (_,s') -> run (many' par) s'

-- run (many (char 'a')) "aaax" --> Just ((),"x")
-- run (many (char 'a')) "x" --> Just ((),"x")
-- run (many (char 'a')) "ax" --> Just ((),"x")

-- many (many (char 'a'))   vegtelen ciklus

instance Alternative Parser where
  empty = Parser $ const Nothing
  par1 <|> par2 = Parser $ \ s -> case run par1 s of
    Nothing -> run par2 s
    Just (a,s') -> Just (a,s')

-- run (many (char 'a')) "aaax" --> Just ((),"x")
-- run (many (many (char 'a'))) "aaax" --> Just ((),"x")
