
-- | Lenses ("lencsék")
-- Könyv: "Concurrent and Parallel Programming in Haskell", Simon Marlow

{-# language RankNTypes, ScopedTypeVariables, DeriveFunctor, InstanceSigs, FlexibleContexts #-}

import Control.Monad.State


-- motiváció: egymásba ágyazott, struktúrált adatot bejárni, módosítani, feldolgozni
--            hozzáféréseket, bejárásokat modulárisan, erősen típusozottan

-- fst :: (a, b) -> a
-- snd :: (a, b) -> b
--

data Color = Red | Green | Blue
  deriving (Eq, Show)

data Foo = Foo { name :: String, age :: Int, color :: Color}
  deriving (Eq, Show)

f1 :: (Color -> Color) -> (Foo, Foo, Foo) -> (Foo, Foo, Foo)
f1 f (Foo n a c, f2, f3) = (Foo n a (f c), f2, f3)

-- HTML, JSON, XML

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- Either esetén traverse Right-ot bejárja

_Left :: Applicative f => (a -> f b) -> Either a c -> f (Either b c)
_Left f (Left a)  = Left <$> f a
_Left f (Right c) = pure (Right c)

_Right :: Applicative f => (a -> f b) -> Either c a -> f (Either c b)
_Right = traverse

_Just :: Traversal (Maybe a) (Maybe b) a b
_Just f (Just a) = Just <$> f a
_Just f Nothing  = pure Nothing


-- bejárások ("Traversal") 0 vagy több értéket érnek el egy struktúra belsejében

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

 -- Traversal s t a b : "s" belsejében elérünk 0 vagy több "a" típusú adatot,
 --                     és ha az "a" adatokat "b"-re változatjuk, akkor "t" lesz
 --                     a külső struktúra típusa

type Traversal' s a = Traversal s s a a

-- lencse: bejárás, viszont *pontosan* egy értéket tudunk mindig elérni
--         egy lens az egy konkrét rész-értékre "fókuszál" egy struktúrában

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- rögzített típusú lencse
type Lens' s a = Lens s s a a


-- első pár mező elérése
_1 :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
_1 f (a, c) = (\b -> (b, c)) <$> f a

_2 :: Functor f => (a -> f b) -> (c, a) -> f (c, b)
_2 f (c, a) = (\b -> (c, b)) <$> f a

_name :: Lens' Foo String
_name f (Foo n a c) = (\n -> Foo n a c) <$> f n

_age :: Lens' Foo Int
_age f (Foo n a c) = (\a -> Foo n a c) <$> f a

_color :: Lens' Foo Color
_color f (Foo n a c) = (\c -> Foo n a c) <$> f c

-- getter/setter függvények lencsékre

-- bemelegítés: hogy oldom meg, hogy egy traversal-al csak simán map-elek (mellékhatás nélkül)?
--    az "identitás" Applicative-al hívok meg traversal-t

newtype Id a = Id {unId :: a} deriving Functor

instance Applicative Id where  -- nincs mellékhatás
  pure = Id
  Id f <*> Id a = Id (f a)

type Setter s t a b = (a -> Id b) -> s -> Id t

over :: Setter s t a b -> (a -> b) -> s -> t
over trav f s = unId (trav (\a -> Id (f a)) s)

  -- minden Traversal egyben Setter is
  -- minden Lens egyben Setter is

-- konstans funktor segítsével tudunk "lekérdezni" adatot

newtype Const a b = Const {unConst :: a} deriving Functor

type Getter r s a = (a -> Const r a) -> s -> Const r s

view :: Getter a s a -> s -> a
view l s = unConst (l Const s)

firstColors :: Traversal' [(Foo, Int)] Color
firstColors = traverse . _1 . _color


newtype Endo a = Endo {unEndo :: a -> a}

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id


instance Monoid a => Applicative (Const a) where
  pure :: b -> Const a b
  pure _ = Const mempty

  (<*>) :: Const a (b -> c) -> Const a b -> Const a c
  Const x <*> Const y = Const (x <> y)

foldrOf :: Getter (Endo b) s a -> (a -> b -> b) -> b -> s -> b
foldrOf l f b s = unEndo (unConst (l (\a -> Const (Endo (\b -> f a b))) s)) b

toListOf :: Getter (Endo [a]) s a -> s -> [a]
toListOf l s = foldrOf l (:) [] s

-- Csak azokat az értékek járjuk be, amire (a -> Bool) igaz.
filtered :: (a -> Bool) -> Traversal' a a
filtered f trav a = if f a then trav a else pure a

-- map (+10) $ filter even [0..10]
--   helyett

-- over (traverse . filtered even) (+10) [0..10]

-- State monád szépséghibája: State s-ben vagyunk, akkor get/put/modify s-re vonatkozik
--   mi van, ha "s" rekord típus, és bizonyos mezőket akarunk csak módosítani
--   lencsékkel jól megoldható

--
------------------------------------------------------------

(%=) :: Setter s s a b -> (a -> b) -> State s ()
(%=) f g = modify (over f g)
infix 4 %=

(.=) :: Setter s s a b -> b -> State s ()
(.=) f b = f %= const b
infix 4 .=

use :: Getter a s a -> State s a
use f = view f <$> get

(&~) :: s -> State s a -> s
(&~) s f = execState f s
infixl 1 &~

p1 :: State Foo ()
p1 = do
  _age   %= (+10)
  _color .= Red
  _name  %= (++"!")
  x <- use _age
  undefined

-- beépített rekord módosítás szintaxis
--
f2 :: Foo -> Foo
f2 foo = foo { age = age foo + 10 }

-- lens (+ State)
--   mező-elérés: típusos, kifaktorálható, exportálható, kombinálhatő
--   rekord-módosítás szintén


{-

--------------------------------------------------------------------------------

newtype Id a = Id {unId :: a} deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id a = Id (f a)

newtype Const a b = Const {unConst :: a} deriving Functor

instance Monoid a => Applicative (Const a) where
  pure _              = Const mempty
  Const x <*> Const y = Const (x <> y)

newtype Endo a = Endo {unEndo :: a -> a}

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &


--------------------------------------------------------------------------------

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)

type Traversal' s a = Traversal s s a a

type Setter s t a b = (a -> Id b) -> s -> Id t

type Getter r s a = (a -> Const r a) -> s -> Const r s

--------------------------------------------------------------------------------

over :: Setter s t a b -> (a -> b) -> s -> t
over f g s = unId (f (Id . g) s)

(%~) = over
infixr 4 %~

set :: Setter s t a b -> b -> s -> t
set f b s = over f (const b) s

(.~) = set
infixr 4 .~

view :: Getter a s a -> s -> a
view f s = unConst (f Const s)

(^.) = view
infixl 8 ^.

foldrOf :: Getter (Endo b) s a -> (a -> b -> b) -> b -> s -> b
foldrOf l f b s = unEndo (unConst (l (\a -> Const (Endo (\b -> f a b))) s)) b

toListOf :: Getter (Endo [a]) s a -> s -> [a]
toListOf l s = foldrOf l (:) [] s

(^..) :: s -> Getter (Endo [a]) s a -> [a]
(^..) s f = toListOf f s
infixl 8 ^..

filtered :: (a -> Bool) -> Traversal' a a
filtered f g a = if f a then g a else pure a

_1 :: Lens (a, c) (b, c) a b
_1 f (a, c) = (\b -> (b, c)) <$> f a

_2 :: Lens (c, a) (c, b) a b
_2 f (c, a) = (\b -> (c, b)) <$> f a

_Left :: Traversal (Either a c) (Either b c) a b
_Left f (Left a) = Left <$> f a
_Left f (Right c) = pure (Right c)

_Right :: Traversal (Either c a) (Either c b) a b
_Right f (Left c) = pure (Left c)
_Right f (Right a) = Right <$> f a

_Nothing :: Traversal' (Maybe a) ()
_Nothing f Nothing = Nothing <$ f ()
_Nothing f (Just a) = pure $ Just a


-- State
--------------------------------------------------------------------------------

(%=) :: Setter s s a b -> (a -> b) -> State s ()
(%=) f g = modify (over f g)
infix 4 %=

(.=) :: Setter s s a b -> b -> State s ()
(.=) f b = f %= const b
infix 4 .=

use :: Getter a s a -> State s a
use f = view f <$> get

(&~) :: s -> State s a -> s
(&~) s f = execState f s
infixl 1 &~

-}
