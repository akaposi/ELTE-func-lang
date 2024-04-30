{-
parametricity

John Reynolds: Types, abstraction and parametric polymorphism, 1984   <- System F
- Descartes: 
- Bessel

f :: a -> a              x::a ∧ P(x)  ⇒  P(f x)         f :: Id ̇→ Id,    f :: forall a. Id a -> Id a
                                (x=3) ⇒  (f x=3)        g(f x) = Id g(f x) = f (Id g x) = f (g x)

f :: a -> a -> a         x,y::a ∧ P(x)        ∧ P(y)        ⇒ P(f x y)
f u v =                           (x=u ∨ x=v)  (y=u ∨ y=v)    (f x y=u ∨ f x y=v)

f :: a->(a->a)->a        x::a, g::a->a, P(x), (∀y.P(y)⇒P(g y))  ⇒ P(f x g)
                     R::a->b->*, x::a, y::b, R(x,y), g::a->a,h::b->b,(∀x y.R(x,y)⇒R(g x,h y))⇒R(f x g,f y h)

f :: forall s . (s -> Int -> s) -> (s -> (Int,s)) -> (s -> Bool) -> s -> s
f push pop isEmpty init = ...
s,s'::*,     R:s->s'->*
push,push'   push R push'

representation-independence, abstraction, parametricity, naturality

F :: * -> *
F a = (a -> a) -> a
F nem funktor

F :: *×*ᵒᵖ -> *
F (a,b) = (a -> b) -> a
functor, 


F(f,g) :: F(a,b') -> F(a',b)
F(f,g) h := \i.f(h(\x.g(i(f x))))
  f :: a->a'
  g :: b->b'
  h :: (a -> b') -> a
  i :: a' -> b
  f(h(\x.g(i(f x)))) :: a'
    x :: a

dinatural transformation: α :: forall a. F(a,a) -> G(a,a),       natural trafo α :: F a -> G a
  (f,g)
                     α
            F(a',b') --> G(a',b')                                         α
              ^            \                                         F a ---> G a
      F(f,id)/              \F(id,g)                                  |        |
            /                v                            fmap f = F f|        |G f = fmap f 
        F(a,b')              F(a',b)                                  v   α    v
             \                ^                                      F b ---> G b
       F(id,g)\              /F(f,id)     
               v     α      /             
             F(a,b) --> G(a,b)

strong dinatural trafo
-}

-- jovo ora 39 perccel rovidebb (ez mar kumulalt ertek)

-- typechecker
-- compiler
