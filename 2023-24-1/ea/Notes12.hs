infixl 4 :$:

data Λ = Var String | Λ :$: Λ | Lam String Λ
-- β : Lam "x" t :$: u = t-ben az "x" osszes elofordulasat u-ra helyettesitem
-- pl. Lam "x" (Var "x" :$: Var "y") :$: Var "z" = Var "z" :$: Var "y"

id :: Λ
id = Lam "x" (Var "x")

k :: Λ
k = Lam "x" (Lam "y" (Var "x"))

s :: Λ
s = Lam "f" (Lam "g" (Lam "x" (Var "f" :$: Var "x" :$: (Var "g" :$: Var "x"))))

-- Λ                 ---->  (valami -> valami)
-- Lam "x" (Var "x") |--->  (\ x -> x)   (evaluator, metacircular interpreter)

-- Λ -> Λ

-- ez igy nehez:
eval :: Λ -> Λ
eval (Var s) = Var s
eval (Lam s t) = Lam s (eval t)
eval (Lam s t :$: u) = undefined -- "t-ben s-eket u-ra cserelni"

data Tm = Var' String | Tm :$$: Tm | Lam' (Tm -> Tm)
-- higher order abstract syntax

instance Show Tm where
  show :: Tm -> String
  show (Var' s) = s
  show (t :$$: u) = "(" ++ show t ++ " " ++ show u ++ ")"
  show (Lam' f) = "lambda"

id' :: Tm
id' = Lam' (\x -> x)

k' :: Tm
k' = Lam' $ \x -> Lam' $ \ _ -> x

eval' :: Tm -> Tm
eval' (Var' s) = Var' s
eval' (Lam' f) = Lam' (\x -> eval' (f x))
eval' (t :$$: u) = case eval' t of
  (Lam' f) -> eval' (f (eval' u))
  t'       -> t' :$$: eval' u

-- fuzz-testing

-- generikus programozas

-- tipus meg van adva egy f-el

-- f :: * -> *
data Fix f = Mk (f (Fix f))

-- List a-ra: f x = Either Unit (a,x)
-- List a = f(f(f(f(f(...)...)
-- List a = Either Unit (a,List a)
-- List a = f (List a)

newtype ListF a x = MkF (Either () (a,x))
  deriving (Functor)

-- Functor (\_-> a)
-- Functor f, Functor g, akkor Functor (\x -> Either (f x) (g x))
-- Functor f, Functor g, akkor Functor (\x -> (f x,g x))
-- Contravariant (\_-> a)
-- Contravariant f, Contravariant g, akkor Contravariant (\x -> Either (f x) (g x))
-- Contravariant f, Contravariant g, akkor Contravariant (\x -> (f x,g x))
-- Contravariant f, Functor g, akkor Functor (\x -> (f x -> g x))
-- Contravariant g, Functor f, akkor Contravariant (\x -> (f x -> g x))

newtype FF x = MkFF ((x -> x) -> x)
-- newtype FF x = MkFF ((x -> Int) -> x)
  deriving Functor
-- Functor (\x -> x), Contravariant (\_->Int) , akkor Contravariant (\x -> x -> Int)

-- Cedille

type List a = Fix (ListF a)
nil :: List a
--     Mk (ListF a (Fix (ListF a)))
--     Mk (Either () (a,Fix (ListF a)))
nil = Mk (MkF (Left ()))
cons :: a -> List a -> List a
cons x xs = Mk (MkF (Right (x,xs)))

map' :: (a -> b) -> List a -> List b
map' f (Mk(MkF (Left ())))      = nil
map' f (Mk(MkF (Right (x,xs)))) = cons (f x) (map' f xs)
-- pattern synonym-okkal ezt talan lehet szepiteni

instance (Show a) => Show (List a) where
  show (Mk(MkF (Left ()))) = "[]"
  show (Mk(MkF (Right (x,xs)))) = show x ++ ":" ++ show xs
  
readLn = 3

-- List a konstruktora
mkList :: ListF a (Fix (ListF a)) -> Fix (ListF a)
mkList = Mk

-- altalanossagban, tetszoleges f :: * -> * -ra (ami egy tipust kodol el)
mk :: f (Fix f) -> Fix f
mk = Mk

-- mkList :: Either () (a,[a]) -> [a]

-- mkFix-et olytan f-ekre tudjuk megadni, melyek "polinomialasak"

-- fold :: (f x -> x) -> Fix f -> x
-- fold :: (Either () (a,x) -> x) -> [a] -> x
foldList :: (Either () (a,x) -> x) -> [a] -> x
foldList alg [] = alg (Left ())
foldList alg (a:as) = alg (Right (a,foldList alg as))
-- == foldr

fold :: Functor f => (f x -> x) -> Fix f -> x
fold alg (Mk w) = alg (fmap (fold alg) w)
-- w :: f (Fix f)
-- ? :: x
-- valami (fold alg) w
-- fold alg :: Fix f -> x
-- fmap (fold alg) :: f (Fix f) -> f x
-- fmap (fold alg) w :: f x
-- alg (fmap (fold alg) w) :: x

main = putStrLn "hello"

-- breadth first tree labelling

α :: Int
α = 3

-- parser folytatasa

-- Traversable
-- map
-- traverse :: (a -> Maybe b) -> [a] -> Maybe [b]   -- Maybe is Applicative
-- pred :: Int -> Maybe Int
-- sequenceA

{-
label :: (Traversable t) => t a -> t (a, Int)
label t = evalState (traverse f t) 0 where
  f a = do
    n <- get
    put (n + 1)
    pure (a, n)
-}

-- Alternative
-- Maybe
-- List

-- ST monad

-- a kovetkezo ora 37 perccel rovidebb
