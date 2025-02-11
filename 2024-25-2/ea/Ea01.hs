{-
Funkcionalis nyelvek     = Halado Haskell
Funkcionalis programozas = Kezdo Haskell

Kaposi Ambrus    akaposi@inf.elte.hu
docens, IK, Progr.Nyelvek es Forditoprogramok

16.00--16.45, 16.50--17.35

lehet kerdezni, nem kell jelentkezni, lehet bekiabalni

Neptunban helyes koddal kell felvenni, BSc:18
gyakorlat Neptunban legyen ugyanaz, mint a valosagban

http://github.com/akaposi/ELTE-func-lang/2024-25-2/
- korabbi felevek tananyaga
- nemi szoveges jegyzet: ELTE-func-lang/2022-23-2/ea/

tananyag:

Haskell = tiszta statikus eros tipusos lusta funkcionalis programozasi nyelv, first-class mellekhatasokkal
 * formalis nyelv (= algebrai elmelet)  f(u)
 * programozasi (formalis) nyelv
 * funkcionalis: egy program az nem utasitasok sorozata, hanem egy kifejezes
   Lisp, ML, OCaml, Erlang, F#
   Scala, Agda, Coq, Haskell  <- el van kulonitve az utasitas es kifejezes
 * lusta (Miranda, Clean)
   kiertekelesi strategiak:
   * strict(szigoru, moho, call by value, CBV),
   * call by name(lusta,lazy, CBN)
   * call by need(lusta,lazy)
     CBV: (\x -> x + x) (1+print 2) ~~~> (\x->x+x) 3 ~~~> 3+3 ~~~> 6
     CBN: (\x -> x + x) (1+2) ~~~> (1+2)+(1+2) ~~~> 3+(1+2) ~~~> 3+3 ~~~> 6
     CBV: (\x -> 20) (1+2) ~~~> (\x->20) 3 ~~~> 20
     CBN: (\x -> 20) (1+2) ~~~> 20
          (\x -> x+x) 10  ugyanolyan jok
          (\x -> x + 2) (1+2)  ugyanolyan jok
     call by need: a ketto jo tulajdonsagait egyesiti
     - gyakorlatban: memory leak
     - elmeletben: meg mindig ertjuk
     John Hughes: Why functional progr matters?
     Simon Peyton-Jones: 
   * tipusos: dinamikus(Python,Javascript,Ruby,PHP)/statikus(Java,C++,C#,Haskell,Go)
     erosen tipusos: keves backdoor
     pl. Kotlin erosebb, mint a Java
     Haskell = Kotlin (erossegi szempont)
     tiszta = nagyon-nagyon-nagyon eros
     Haskellben backdoor-ok:
     - f :: Bool
       f lehet True, False, undefined(exception), vegtelen ciklus (f = f), kiirhat a kepernyore(ghc-specific: unsafePerformIO : IO a -> a)
     David Turner: elementary Strong functional programming
   * first-class mellekhatas: a legjobb imperativ programozasi nyelv
-}

-- parametrikus ADTk (, , Either, ->), case, let kötések, Eq, Show, Ord

-- felsorolasos
data Egtaj = E | D | K | Ny

eszak :: Egtaj
eszak = E

instance Show Egtaj where
  show :: Egtaj -> String
  show e = "menj haza"

data Nat = Zero | Suc Nat

instance Show Nat where
  show n = show (f n)
    where
      f :: Nat -> Int
      f Zero = 0
      f (Suc n) = 1+f n

harom = Suc $ Suc $ Suc Zero

data Maybe' a = Nothing | Just a

data List a = Nil | Cons a (List a)

egykettoharom :: List Int
egykettoharom = Cons 1 $ Cons 2 $ Cons 3 Nil

-- kov. ora 1 perccel rovidebb
