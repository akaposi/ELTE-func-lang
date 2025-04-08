{-# LANGUAGE ImpredicativeTypes #-}
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

data Tree = Leaf Int | Node Tree Tree
  deriving (Show)

{-
  Node
    (relabel (i+1) t1)
    (relabel (i+?) t2)
-}  

t1 = Node (Node (Leaf 0) (Leaf 0)) (Node (Leaf 0) (Leaf 0))


--       :: Tree -> Tree
relabel' :: Tree -> WriterT String (State Int) Tree
relabel' (Leaf _) = do
  i <- get
  put (i+1)
  tell $ show i ++ "-et megnovolem 1-el\n"
  return (Leaf i)
relabel' (Node t1 t2) = do
  t1' <- relabel' t1
  t2' <- relabel' t2
  return (Node t1' t2')

-- get :: State s s
-- put :: s -> State s ()

-- runState (runWriterT $ relabel' t1) 0

-- 3 + 2 + 6 rovidebb



-- logika Haskellben

-- Agda    -- elsorendu logika (∧,∨,⇒,¬,⊤,⊥,∀,∃)
-- Haskell -- iteletlogika     (∧,∨,⇒,¬,⊤,⊥)

-- Raymond Smullyan

-- (a ∧ (b ⇒ c) ∧ (a ⇒ b)) ⇒ c
biz :: forall a b c . (a,b->c,a->b) -> c
biz (a,bc,ab) = bc (ab a)

-- tipusellenorzes = bizonyitasellenorzes

type True = forall a. a -> a
type False = forall a . a
type a :/\ b = (a,b)
type a :<-> b = (a->b) :/\ (b->a)
type a :\/ b = Either a b
type Not a = a -> False

l2 :: (p -> q -> r) -> (q -> p -> r)
l2 = \pqr q p -> pqr p q

l3 :: p :<-> (True :/\ p)
l3 = (\p->(\a->a,p),snd)

l4 :: p :<-> (False :\/ p)
l4 = (Right,\fp->case fp of
                   Left f -> f
                   Right p->p)

l5 :: (p -> q :/\ r) :<-> ((p -> q) :/\ (p -> r))
l5 = (\pqr->(fst . pqr,snd . pqr),\(pq,pr) p->(pq p,pr p))

l6 :: (p :\/ q -> r) :<-> ((p -> r) :/\ (q -> r))
l6 = (\pqr->(pqr.Left,pqr.Right),\(pr,qr) pq->case pq of
                                                Left  p->pr p
                                                Right q->qr q)

l7 :: p -> Not (Not p)
--    p -> (p -> False) -> False
-- l7 = \p pf -> pf p
l7 = flip ($)
-- flip :: (a -> b -> c) -> b -> a -> c
-- flip :: ((a -> b) -> a -> b) -> a -> (a->b) -> b

-- law of excluded middle, kizart harmadik elve, tertium not datur
nincs :: forall p . p :\/ Not p
--       forall p . Either p (p -> False)
nincs = undefined

nincs' :: forall p.Not (p :\/ Not p)
--       (forall p . Either p (p -> False)) -> False
nincs' = undefined

-- idoutazas
l8 :: Not (Not (p :\/ Not p))
--    (Either p (p->False) -> False) -> False
l8 = \g->g (Right (\p->g (Left p)))
-- ? :: False
-- g :: Either p (p->False) -> False
-- p :: p

-- DNP = double negation principle
l9 :: Not (Not (Not (Not p) -> p))
l9 = undefined

l10 :: Not (Not (Not p)) :<-> Not p
l10 = undefined

-- f functor, fmap :: (a->b)->f a->f b
-- g contra,  contramap :: (a->b)->g b->g a

-- f functor    g contra
-- ---------------------
-- (a ↦ g a -> f a) functor
-- (a ↦ f a -> g a) contra

class Contra g where
  contramap :: (a->b)->g b->g a

data Fun f g a = Fun (f a -> g a)

instance (Functor f,Contra g) => Contra (Fun f g) where
  contramap :: (a->b) -> Fun f g b -> Fun f g a
  --           (a->b) -> (f b->g b)-> (f a->g a)
  contramap h (Fun i) = Fun (contramap h . i . fmap h)
  -- fmap h      :: f a -> f b
  -- i           :: f b -> g b
  -- contramap h :: g b -> g a
  -- ?           :: f a -> g a

instance (Contra f,Functor g) => Functor (Fun f g) where
  fmap h (Fun i) = Fun (fmap h . i . contramap h)
  -- contramap h :: f b -> f a
  -- i           :: f a -> g a
  -- fmap h      :: g a -> g b
  -- ?           :: f b -> g b

-- Functor (a ↦ a)
-- Functor (a ↦ Int)
-- Contra  (a ↦ Int)

data Test a = Test ((((a->Int)->(Int,a))->[Int])->a)
  deriving Functor

data Test1 a = Test1 ((a->(a->Int))->Int)
  deriving Functor

-- data Test2 a = Test2 (a->a)
--   deriving Functor

-- 17.17

data DN a = DN { unDN :: Not (Not a) }
  deriving Functor

instance Applicative DN where
  pure :: a -> DN a
  pure a = DN (flip ($) a)
  (<*>) :: DN (a -> b) -> DN a -> DN b
  DN f <*> DN g = DN (\h -> f (\ab->g(\a->h(ab a))))
  -- ? :: False
  -- h :: b -> False
  -- g :: (a->False)->False
  -- f :: ((a->b)->False)->False
  -- ab : a->b

instance Monad DN where
  (>>=) :: DN a -> (a -> DN b) -> DN b
  DN f >>= g = DN (\h -> f (\a->unDN (g a) h))
  -- f :: (a->False)->False
  -- g :: a -> (b->False) -> False
  -- h :: b->False
  -- ? :: False

l11 :: Not (p :<-> Not p)
l11 = undefined

l12 :: (Not p :\/ q) -> (p -> q)
l12 = undefined

l13 :: (Not (p :\/ q)) :<-> (Not p :/\ Not q)
l13 = undefined

l14 :: (Not p :\/ Not q) -> Not (p :/\ q)
l14 = undefined
