
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


-- Feladatok. Alkalmazzuk a (>>=) és (<$>) függvényeket ahol célszerű!


-- Írjuk meg a következő függvényt úgy, hogy ha a bement tartalmaz "a"-t,
-- akkor a kimenet is tartalmazzon.
f1 :: Maybe (Maybe a) -> Maybe a
f1 = undefined


-- Alkalmazzuk mindhárom input függvényt az (a, b, c) hármas
-- elemeire. Ha bármelyik függvény Nothing-ot ad, az eredmény legyen Nothing,
-- egyébként Just.
f2 :: (a -> Maybe a') -> (b -> Maybe b') -> (c -> Maybe c')
   -> (a, b, c) -> Maybe (a', b', c')
f2 = undefined

-- Legyen a kimenet Nothing ha bármelyik bemenet listaelem Nothing,
-- egyébként Just <input elemek listája>.
f3 :: [Maybe a] -> Maybe [a]
f3 = undefined


-- emlékeztető: alkalmazzuk a függvényt páronként a kapott listák
-- elemeire! Ha a listák hossza nem azonos, akkor hagyuk el a hosszabb
-- lista kilógó elemeit. Pl zipWith' (+) [1, 1, 1] [1, 1] == [2, 2]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' = undefined


-- Alkalmazzuk páronként a kapott (a -> b -> Maybe c) függvényt
-- a bemenő listák elemeire! Ha bármelyik függvényalkalmazás Nothing,
-- akkor a kimenet legyen Nothing, egyébként Just <lista zippelés eredménye>
zipWithMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMaybe = undefined



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

eval :: Exp -> Maybe (Either Int Bool)
eval = undefined
