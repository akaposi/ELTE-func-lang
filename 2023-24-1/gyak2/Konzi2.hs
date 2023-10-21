module Konzi2 where

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x : xs) = f x : mapList f xs

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

-- ez a művelet álatlánosíthatónak tűnik
-- mi lenne egy ilyen művelet típusszignatúrája?
{-

  map :: (a -> b) -> ?? -> ??

pl [a], [b]
tehát simán ezeket oda nem lehet beírni
"konextusfüggő"

Magasabbrendű polimorfizmus
Mi van ha nem csak típusok fölött tudnánk polimorfizálni / általánosítani
Hanem TÍPUSFÜGGVÉNYEK fölött is
típuskonstruktor
|
V
Maybe :: Type -> Type
[_]   :: Type -> Type
NoNempty :: Type -> Type

ezekre igazából tudnánk ilyen map műveletet írni

  típuskonstruktor
     | pontosan egy típusparaméter
     V V
data X a = Y | Z | A | B | ...

f :: Maybe
f = ????

* = Type
Functor az * -> * kifejezésekre ad megkötést
-}

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- Struktúra prezerválás
-- Ha egy konstruktorra mintaillesztünk akkor a jobboldalt az meg fog jelenni
-- Mindig pontosan 1x kell mintailleszteni

-- A paraméterekkel mit csináljunk?
-- Begyünk barra sorba a paramétereken
-- Mi a típusa?
-- t :: a => h t (ahol h az fmap által paraméterül kapott fv)
-- t :: pl Int (b, Bool, csak az a lényeg az a ne szerepeljen benne) => t
-- t :: g a (tehát a megjelenik a típusban, de nem simán a van) => fmap h a => ha nem tudunk fmapolni rajta, akkor kicsit jobban meg kell vizsgálni

instance Functor' [] where
  fmap' :: (a -> b) -> [a] -> [b]
  fmap' h [] = [] -- step 0: leírjuk a konstruktort, step 1: végig megyünk a konstruktor paraméterein, de mivel nincs ilyen ezért megvagyunk
  fmap' h (x : xs) = h x : fmap' h xs -- <-- sima fmapot írj
    -- step 0: leírjuk a konstruktort
    -- step 1: végig megyünk a paramétereken, BALRÓL JOBBRA
    -- step 2: nézzük x-et, mi x típusa?
    -- válasz: x :: a, konzultáljuk a guide-ot fent
    -- step 3: nézzük xs-t, mi xs típusa?
    -- válasz:  xs :: [a], konzultájuk a guide-ot
    -- nincs több paraméter kész vagyunk

             --    V típuskonstruktor
instance Functor' Maybe where
  fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap' h Nothing = Nothing
  fmap' h (Just a) = Just (h a)

-- Either
-- Either típusa mi mint típuskonstruktor?
-- két db típusparamétert vár
-- pl Either Int Bool
-- Either :: * -> * -> *
-- Viszont ahhoz hogy Functor legyen valami, ahhoz * -> * kell nekünk
-- Ezért "le kell fixálnunk" az első paramétert

-- data Either q a = Left q | Right a
instance Functor' (Either q) where
  fmap' :: (a -> b) -> Either q a -> Either q b -- q nem fog változni az fmap működése során
  fmap' h (Left q1) = Left q1 -- q1 :: ?, Left q1 :: Either q a => q1 :: q, q mint típus FÜGGETLEN a-tól
  fmap' h (Right a1) = Right (h a1) -- a1 :: a => h a1

data FlippedEither q a = FLeft a | FRight q

instance Functor' (FlippedEither q) where
  fmap' :: (a -> b) -> FlippedEither q a -> FlippedEither q b
  fmap' h (FLeft a) = FLeft (h a)
  fmap' h (FRight q) = FRight q

-- "Nagyobb magasabbrendű polimorfizmus"
-- Magsabbrendű polimorfizmus => * -> * típusok fölött polimorfizáltunk
-- De nem muszály itt megálni
-- * -> * -> *

-- f :: * -> *             f a :: *, a :: ??, f :: ?? -> *, de nálunk a ?? mindig típus lesz, tehát f :: * -> *, a :: *
-- a :: *                  V
data Apply f a = MkApply (f a)

--                Apply :: (* -> *) -> * -> *, Apply f :: * -> *
instance Functor' f => Functor' (Apply f) where
  fmap' :: (a -> b) -> Apply f a -> Apply f b
  fmap' h (MkApply fa) = MkApply (fmap' h fa) -- h :: a -> b, fa :: f a <- 'a' nem magában jelenik meg => fmap h fa
  -- ahhoz, hogy mi az fmap-ot valami fa-ra tudjuk alkalmazni, az f-nek Functornak kell lennie, tehát kell ez az előkövetelmény

-- Compose :: (* -> *) -> (* -> *) -> * -> *
data Compose f g a = MkCompose (f (g a))

instance (Functor' f, Functor' g) => Functor' (Compose f g) where
  fmap' :: (a -> b) -> Compose f g a -> Compose f g b
  fmap' h (MkCompose fga) = MkCompose (fmap' (\ga -> fmap' h ga) fga) -- fga :: f (g a), ebben az 'a' benne van ezért fmap f fga-t megpróbáljuk
