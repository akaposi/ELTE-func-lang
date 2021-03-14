

-- IO feladatok.
--------------------------------------------------------------------------------


-- Olvass be egy sort stdin-ről, majd írd ki a sor hosszát (karaketerek száma).
io1 :: IO ()
io1 = do
  l <- getLine
  print $ length l

io1' :: IO ()
io1' = getLine >>= \l -> print $ length l

io1'' :: IO ()
io1'' = getLine >>= print . length
  -- print . length =<< getLine            -- print(length(getLine()))

-- Addig olvass be ismételten sorokat stdin-ről, amíg a sor nem tartalmaz 'x' karaktert.  Ha a
-- sorban 'x' van, akkor nyomtasd ki az összes eddig beolvasott sort.

io2 :: IO ()
io2 = go [] where
  go lines = do
    l <- getLine
    if elem 'x' l then do
      print lines
    else
      go (lines ++ [l])

io2' :: IO ()
io2' = go [] where
  go lines =
    getLine >>= \l ->
    if elem 'x' l then
      print lines
    else
      go (lines ++ [l])

-- Olvass be egy sort, majd annyiszor olvass be új sort, ahány karakter van a sorban.
io3 :: IO ()
io3 = do
  l <- getLine

  let go 0 = pure ()
      go n = getLine >> go (n - 1)

  go (length l)

io3' :: IO ()
io3' =
  getLine >>= \l ->

  let go 0 = pure ()
      go n = getLine >> go (n - 1)

  in go (length l)

io3''' :: IO ()
io3''' = go . length =<< getLine where
  go 0 = pure ()
  go n = getLine >> go (n - 1)

-- Control.Monad-ból
replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n ma | n <= 0    = pure ()
                 | otherwise = ma >> replicateM_ (n - 1) ma

io3'' :: IO ()
io3'' = do
  l <- getLine
  replicateM_ (length l) getLine

-- Olvass be egy sort, és add vissza a hosszát!
io4 :: IO Int
io4 = do
  l <- getLine
  pure (length l)


io4' :: IO Int
io4' =
  getLine >>= \l ->
  pure $ length l

io4'' :: IO Int
io4'' = fmap length getLine

io4''' :: IO Int
io4''' = length <$> getLine  -- fmap operátorként írva: (<$>)



-- A következőt ismételd végtelenül: olvass be egy sort, majd nyomtasd ki a sorban a kisbetűk számát.
-- A Ctrl-c-c -vel lehet megszakítani a futtatást ghci-ben.
io5 :: IO ()
io5 = do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l
  io5

io5' :: IO ()
io5' =
  getLine >>= \l ->
  (print $ length $ filter (\c -> 'a' <= c && c <= 'z') l) >>
  io5

-- Control.Monad-ból
forever :: Monad m => m a -> m b
forever ma = ma >> forever ma

-- loop :: a    -- forever, mellékhatás nélkül
-- loop = loop

-- harmadik verzió forever-el
io5'' :: IO ()
io5'' = forever $ do
  l <- getLine
  print $ length $ filter (\c -> 'a' <= c && c <= 'z') l
