{-# language BlockArguments, DeriveFunctor, RecursiveDo #-}

import Control.Monad
import Control.Monad.State
import Debug.Trace

-- Téma Javaslat:
--   Cont monád
--   Monád transzformerek
--   ipari parser lib belseje (flatparse)
--   Reverse State (time travel) monád
--   gyakorlati példa, Haskell-ben éri meg csinálni

-- (github.com/AndrasKovacs/normalization-bench)
--  JVM, V8 (nodejs), CLR (.NET), Haskell: interpreter / gépi kódos
--  OCaml: gépi kódos, Scala interpreter
--  lambda normalizálás

-- lambda AST ("first-order syntax")
data Tm = Var String | App Tm Tm | Lam String Tm
 deriving Show

-- "higher-order syntax"

data Val = VVar String | VApp Val Val | L String (Val -> Val)

vid :: Val
vid = L "x" \x -> x

vconst :: Val
vconst = L "x" \x -> L "y" \y -> x

infixl 8 $$
($$) :: Val -> Val -> Val
($$) f x = case f of
  L _ f -> f x
  f        -> VApp f x

-- ($)
vapp :: Val
vapp = L "f" \f -> L "x" \x -> f $$ x

vzero :: Val
vzero = L "s" \s -> L "z" \z -> z

vsuc :: Val
vsuc = L "n" \n -> L "s" \s -> L "z" \z -> s $$ (n $$ s $$ z)

vadd :: Val
vadd = L "n" \n -> L "m" \m -> L "s" \s -> L "z" \z ->
      n $$ s $$ (m $$ s $$ z)

v5 = vsuc $$ (vsuc $$ (vsuc $$ (vsuc $$ (vsuc $$ vzero))))
v10 = vadd $$ v5 $$ v5

fresh :: [String] -> String -> String
fresh xs x = if elem x xs then fresh xs (x++"'")
                          else x

quote :: [String] -> Val -> Tm
quote env v = case v of
  VVar x   -> Var x
  VApp t u -> App (quote env t) (quote env u)
  L x f    -> let x' = fresh env x in
              Lam x' (quote (x':env) (f (VVar x')))


{-

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  -- (>>=) (State f) g = State $ \s -> case f s of
  --   (a, s') -> case runState (g a) s of
  --     (b, s'') -> (b, s'')

  -- (>>=) (State sf) f = State $ \s ->
  --   let (a, past)   = sf future
  --       (b, future) = runState (f a) s
  --   in (b, past)

  (>>=) (State f) g = State $ \s ->
    let (a, past)   = f future
        (b, future) = runState (g a) s
    in (b, past)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

foo :: State [Int] ()
foo = do
  modify (take 100)
  xs <- get
  put (0:xs)
  pure ()

-}

foo :: State [Int] ()
foo = mdo
  put (0:xs)
  xs <- get
  modify (take 100)
  pure ()

-- class Monad m => MonadFix m where
--   mfix :: (a -> m a) -> m a

fix' :: (a -> a) -> a
fix' f = let x = f x in x

fix'' :: (a -> a) -> a
fix'' f = f (fix'' f)
    -- fix' f = f (f (f (f (f ( ......... ∞ )))))

-- hogyha van "fix" függvény, akkor azzal minden rekruzív definíció
-- helyettesíthető

factBody :: (Int -> Int) -> (Int -> Int)
factBody = \recurse n -> case n of
  0 -> 1
  n -> n * recurse (n - 1)

fact :: Int -> Int
fact = fix $ \recurse n -> case n of
  0 -> 1
  n -> n * recurse (n - 1)

logFix :: (a -> a) -> a
logFix f = f (trace "HELLO" $ logFix f)


fibBody :: (Int -> Int) -> Int -> Int
fibBody recurse n = case n of
  0 -> 0
  1 -> 1
  n -> recurse (n - 2) + recurse (n - 1)

-- Pozitív Int-ek
memofix :: ((Int -> Int) -> (Int -> Int)) -> Int -> Int
memofix f n =
  let list = map (f (list!!)) [0..]
  in list !! n

-- tetszőleges ADT-t automatikusan memoizálni

-- data Exp = .... | ... | ... | Int | ...

-- memofix :: ((Exp -> Exp) -> (Exp -> Exp)) -> Exp -> Exp

-- persistent data structures (immutable)

-- opcionális:
data Tree = Leaf | Node Tree Tree

-- Kérdés: mi az az adatszerkezet, aminek a kulcsa Tree, értéke tetszőleges típusú
-- insert és lookup költsége lineáris a Tree méretében.

fib = memofix fibBody

fibSlow n = case n of
  0 -> 0
  1 -> 1
  n -> fibSlow (n - 2) + fibSlow (n - 1)
