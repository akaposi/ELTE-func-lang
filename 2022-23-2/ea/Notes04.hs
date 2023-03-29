
-- 1. több a Functor-ról:
--    Functor használata a generikus programozásban
--    (3)

-- 2. Monad osztály (4)

-- 3. type family (--> type-level hacking) (3)

-- 4. GHC pipeline
--     (+ Template Haskell) ("makrók" a GHC-ben) (5)

-- 5. (Typed Template Haskel


-- GHC pipeline
------------------------------------------------------------

{-
Fő kérdések: hogy fordítunk gép kódra:
- polmorfizmust
- első osztályú függvényeket
- lusta kiértékelés
- típusosztályokat
-}

{-
  Forráskód

     ↓ (parser) (parser generátor: Happy, lexer generátor: Alex)
       (operátor parsolás: első pass: precedenciák figyelmen
                           kívül hagyva
                           második pass: átrendezzük az AST-t

  -- if_then_else_ = ....

  -- then : Bool
  -- then = True

  -- else : Bool
  -- else = False

  -- myDef = if else then then else else

  nyers AST (precíz forrás szintaxis)

     ↓ (renaming + scope check + desugaring)

          példa desugaring-re:
            - guard ("őrkifejezés")     (f | cond = ...)
               --> case kifejezés
            - mintaillesztés
               --> case kifejezés
            - let/where
               --> case kifejezés
            - listák, tuple-ök
                [0, 1, 2]
                Data.List.(:) 0 (Data.List.(:) 1 ...)
            - [x + 10 | x <- [0..10]]
                  (három függvényre: concatMap, (\x -> [x])
                                      Control.Monad.guard)
              concatMap (\x -> [x + 10]) [0..10]
              [(x, y) | x <- xs, y <- ys, x /= y]

             flip concatMap xs $ \x ->
             flip concatMap ys $ \y ->
             flip concatMap (if x /= y then [()] else []) $ \_ ->
             [(x, y)]

             Monad instance használatával:
               do x <- xs
                  y <- ys
                  guard (x /= y)
                  pure (x, y)

  desugared AST

       ↓  (típusellenőrzés)
          (leképezés típsozatlan IR-ből
           típusozottba: "elaboration")

          - type inference
          - instance resolution ("mini logikai programozási
                                  feladat")
            (kérdés: lehet két különböző instance ugyanarra
             a típusra?)
            (Haskell-ben: nem

            osztályok fordítása ("elaboration"):

            input:

            class Eq a where
              (==) :: a -> a -> Bool

            instance Eq Bool where

            instance Eq a => Eq (Maybe a) where
               (==) Nothing Nothing = True
               (==) (Just x) (Just y) = x == y
               (==) _ _ = False

            f :: Eq a => a -> a -> Bool
            f x y = x == y

            foo :: Maybe Int -> Maybe Int -> Bool
            foo = f

            output:
              data Eq a = Eq {eq :: a -> a -> Bool}

              eqMaybe :: Eq a -> Eq (Maybe a)
              eqMaybe inst = Eq f where ...

              f :: Eq a -> a -> a -> Bool
              f inst x y = (eq inst) x y

              g :: Bool -> Bool -> Bool
              g x y = eq eqBoolInstance x y

    polimorfizmus fordítása:

      id :: a -> a
      id x = x


    bizonyos nyelvek:
      - "monomorfizáció": minden polimorfizmus eltűnik,
        konkrét példányok vannak
        (C++, Rust)
      - GHC és Java: "type erasure"
          - minden típusnak egységes memóriabeli kiosztása van
          - minden polimorf függvényhez egy kód kell

          - viszont optimalizáció: saját belátása szerint
               monomorfizál

           foo :: Num a => a -> a -> a
           foo x y = x + y

           bar :: Int -> Int -> Int
           bar x y = foo x y

           -- data Int = Int Int#
           --    Int# primitív típus, nem lehet polimorfan
           --      használni

           -- "unboxing": gépi adatot használjuk az
           --   általános objektumok helyett

           -- "worker-wrapper transformation"
           --    egy függvényből kettőt fordítunk

           wbar :: Int# -> Int# -> Int#
           wbar x y = x +# y

           -- (# Int# Int# #)
           -- (# Int# | Int# #)
           -- ByteArray#
           -- Array# a
           -- Array# (# Int#, Int# #)
           -- (Int#, Int#)
           -- MutableArray#

           data Pair (a :: *) (b :: *) = Pair a b
           Int#

     -- Type representation rendszer
     -- type * = TYPE LiftedRep (általános (lusta) objektumok)
     --  TYPE IntRep
     --  (# Int#, Int# #) :: TYPE (TupleRep [ IntRep, IntRep])

  data RuntimeRep
  = VecRep VecCount VecElem
  | TupleRep [RuntimeRep]  --  (# a, b #)
  | SumRep [RuntimeRep]    -- (# a | b #)
  | BoxedRep Levity        -- "dobozolt" objektum
  | IntRep
  | Int8Rep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | WordRep
  | Word8Rep
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | AddrRep
  | FloatRep
  | DoubleRep

  id :: Exp -> Exp
  id a = a

  id :: Exp
  id = [| \x -> x |]



  Core AST  (nyelv neve: GHC Core)
      (típusos lambda kalkulus + ADT + típusnyelv)
     - függvények
     - case kifejezés
     - adattípusok (data)
     - típusok

-}
