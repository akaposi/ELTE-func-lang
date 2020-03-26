
-- Maybe monad folyt.
--------------------------------------------------------------------------------

-- emlékezzünk: Functor és Monad metódusok definíciója Maybe-hez.

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Just a  >>= f = f a
-- Nothing >>= _ = Nothing

-- (<$>) :: (a -> b) -> Maybe a -> Maybe b
-- f <$> Just a  = Just (f a)
-- f <$> Nothing = Nothing

-- pure :: a -> Maybe b
-- pure a = Just a

-- return = pure


-- Feladatok. Alkalmazzuk a (>>=) és (<$>) függvényeket ahol célszerű!


-- Írjuk meg a következő függvényt úgy, hogy ha a bement tartalmaz "a"-t,
-- akkor a kimenet is tartalmazzon.
f1 :: Maybe (Maybe a) -> Maybe a
f1 (Just (Just a)) = Just a
f1 _               = Nothing

f1' :: Maybe (Maybe a) -> Maybe a
f1' mma = mma >>= \ma -> ma
     --   mma >>= id

-- Just ma >>= id = ma
-- Nothing >>= _  = Nothing

f1'' :: Maybe (Maybe a) -> Maybe a
f1'' (Just ma) = ma
f1'' Nothing   = Nothing

-- f1'' :: Monad m => m (m a) -> m a
--  (standard függvény: join)


-- Alkalmazzuk mindhárom input függvényt az (a, b, c) hármas
-- elemeire. Ha bármelyik függvény Nothing-ot ad, az eredmény legyen Nothing,
-- egyébként Just.
f2 :: (a -> Maybe a') -> (b -> Maybe b') -> (c -> Maybe c')
   -> (a, b, c) -> Maybe (a', b', c')
f2 f g h (a, b, c) = case (f a, g b, h c) of
  (Just a', Just b', Just c') -> Just (a', b', c')
  _                           -> Nothing


-- f2' f g h (a, b, c) = case f a of
--   Nothing -> Nothing
--   Just a' -> case g b of
--     Nothing -> Nothing
--     Just b' -> case h c of
--       Nothing -> Nothing
--       Just c' -> Just (a', b', c')

f2' :: (a -> Maybe a') -> (b -> Maybe b') -> (c -> Maybe c')
   -> (a, b, c) -> Maybe (a', b', c')
f2' f g h (a, b, c) =
  f a >>= \a' ->
  g b >>= \b' ->
  h c >>= \c' ->
  pure (a', b', c')
  -- return (a', b', c')

  -- zárójelezve
  -- f a >>= (\a' ->
  -- g b >>= (\b' ->
  -- h c >>= (\c' ->
  -- pure (a', b', c'))))

-- emlékeztető: return és pure tökugyanaz, ajánlásom: használjuk a pure-t
--   pure előnyösebb:
--     - rövidebb
--     - más nyelvekben a return mást jelent


-- Legyen a kimenet Nothing ha bármelyik bemenet listaelem Nothing,
-- egyébként Just <input elemek listája>.
f3 :: [Maybe a] -> Maybe [a]
f3 []       = pure []
f3 (ma:mas) =
  ma     >>= \a ->
  f3 mas >>= \as ->
  pure (a:as)

-- általánosan f3 :: Monad m => [m a] -> m [a]


-- mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
-- f3 = mapMaybe id   -- (házi feladat ezt a definíciót megérteni)


-- emlékeztető: alkalmazzuk a függvényt páronként a kapott listák
-- elemeire! Ha a listák hossza nem azonos, akkor hagyuk el a hosszabb
-- lista kilógó elemeit. Pl zipWith' (+) [1, 1, 1] [1, 1] == [2, 2]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs
zipWith' f _      _      = []
-- zipWith BSc-s anyag


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt
-- a bemenő listák elemeire! Ha bármelyik függvényalkalmazás Nothing,
-- akkor a kimenet legyen Nothing, egyébként Just <lista zippelés eredménye>
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe f (a:as) (b:bs) =
  f a b >>= \c ->
  zipWithMaybe f as bs >>= \cs ->
  pure (c:cs)
zipWithMaybe f _ _ = pure []   -- Just []

-- standard függvény zipWithMaybe
--    neve: zipWithM,  Control.Monad-ból importálható

-- definíció nem használ semmi Maybe műveletet

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f (a:as) (b:bs) =
  f a b >>= \c ->
  zipWithM f as bs >>= \cs ->
  pure (c:cs)
zipWithM f _ _ = pure []

-- példa: zipWithM (\x y -> Just (x + y)) [0..10] [0..10] == Just [0,2,4,6,8,10,12,14,16,18,20]
--        zipWithM (\x y -> if x == 0 then Nothing else Just y) [0..10] [0..10]
--        zipWithM (\x y -> if x == 0 then Nothing else Just y) [1..10] [0..10] == Just [0,1,2,3,4,5,6,7,8,9]

-- példa: zipWithM (\x y -> print (x + y)) [0..10] [0..10]
--           (kinyomtatja a számok összegeit páronként)




-- Tekintsük a következő típust, aminek az elemei egyszerű kifejezésfák:
data Exp = IntLit Int | BoolLit Bool | Add Exp Exp | Not Exp | Eq Exp Exp
  deriving (Show)

-- pl.
e1 = Add (IntLit 0) (IntLit 10) -- 0 + 10
e2 = Eq (IntLit 0) e1           -- 0 == (a + 10)
e3 = Not e2                     -- not (0 == (0 + 10))

-- Írjunk egy függvényt, ami egy Exp kifejezést kiértékel Either Int Bool-ra!
-- A kiértékelés legyen értelemszerű, azaz Add összeadás, Eq egyenlőségvizsgálat,
-- és Not Bool negáció legyen. Minden olyan esetben, ha a kifejezés típushibás,
-- legyen az eredmény Nothing.

-- Példák:
-- eval (Eq (IntLit 0) (BoolLit True)) == Nothing
-- eval (IntLit 10) == Just (Left 10)
-- eval (Add (IntLit 10) (IntLit 20)) == Just (Left 30)
-- eval (Not (IntLit 10)) == Nothing
