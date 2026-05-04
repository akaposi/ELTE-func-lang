
- The first parameter of the `AssignAt` operat# Funkcionális Nyelvek Mintavizsga

A feladatsor megoldására 2.5 óra áll rendelkezésre. Külső segítség és kollaboráció nem megengedett (együttműködés más vizsgázóval, külső személlyel vagy mesterséges intelligenciával : ChatGPT, BingAI stb). A megoldást akárhányszor be lehet küldeni, az utolsó megoldás számít. Részpontszám kapható minden feladatra. A leírás végén megtaláljátok a teljes Haskell fájlt, amit ki kell egészíteni és feltölteni.

Akinek meg van a 28 pontja a gyakorlati számonkérésekből, annak a +1 jegyet automatikusan beszámoljuk, ha a vizsga legalább az eléséges határt eléri.


# Ponthatárok

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Datás feladatok (10 pont)

Adott az alábbi típus:
```hs
data LengthIndexedList i a = Nil | Cons i a (LengthIndexedList i a) deriving (Eq, Show)
```

Írjunk a fenti típusra (algoritmikusan generálható) `Functor`, `Foldable` és `Traversable` instance-ot! __(2 = 0.5 + 0.5 + 1 pont)__

`deriving` semmilyen formában nem használható.

A tesztek során használt példányok:
```hs
l1 :: LengthIndexedList Integer Char
l1 = Cons 11 'h' $ Cons 10 'e' $ Cons 9 'l' $ Cons 8 'l' $ Cons 7 'o' $ Cons 6 ' ' $ Cons 5 'w' $ Cons 4 'o' $ Cons 3 'r' $ Cons 2 'l' $ Cons 1 'd' $ Nil

l2 :: LengthIndexedList Int Bool
l2 = let l@(Cons i a r) = fmap not l2 in Cons (i + 1) True l

l3 :: LengthIndexedList Integer Int
l3 = Cons 2 2 $ Cons 1 1 Nil

```

Tesztek a működésre:
```hs
sum l3 == 3
not $ and l2
toList l1 == "hello world"
fmap (+1) l3 == Cons 2 3 (Cons 1 2 Nil)
traverse (\a -> if a > 2 then Just (a ^ 2) else Nothing) l3 == Nothing
traverse (\a -> if a <= 2 then Just (a ^ 2) else Nothing) l3 == Just (Cons 2 4 (Cons 1 1 Nil))
```

Az általunk definiált listának az `i` típusú paramétere azt reprezentálja, hogy eddig hány elemű a lista. Definiálj egy `satisfyInvariant` függvényt, amely megvizságlja, hogy ez az állítás igaz-e. __(2 pont)__

```hs
satisfyInvariant :: (Num i, Eq i) => LengthIndexedList i a -> Bool
```

Tesztek a működésre:
```hs
satisfyInvariant l1
satisfyInvariant l3
not $ satisfyInvariant $ Cons 2 'a' Nil
not $ satisfyInvariant $ Cons 3 'b' $ Cons 1 'a' Nil
```

Definiálj egy `mkLIL` függvényt, amely egy hajtogatható struktúrából a fenti invariánsnak megfelelő LIL-t generál! __(3 pont)__

```hs
mkLIL :: (Foldable f, Num i) => f a -> LengthIndexedList i a
```

Tesztek a működésre:
```hs
mkLIL "hello world" == l1
mkLIL [1,2,3] == Cons 3 1 (Cons 2 2 (Cons 1 3 Nil))
mkLIL Nothing == Nil
mkLIL "alma" == Cons 4 'a' (Cons 3 'l' (Cons 2 'm' (Cons 1 'a' Nil)))
```

Definiálj egy `reverseLIL` függvényt, amely egy LIL-t megfordít! __(3 pont)__
```hs
reverseLIL :: Num i => LengthIndexedList i a -> LengthIndexedList i a
```

Tesztek:
```hs
reverseLIL (mkLIL "hello") == Cons 5 'o' (Cons 4 'l' (Cons 3 'l' (Cons 2 'e' (Cons 1 'h' Nil))))
reverseLIL (mkLIL [1,2]) == l3
```

## Monád transzformeres feladatok (10 pont)

A feladatok során használt monád transzformer felépíthető típusosztályok használatával vagy a teljes monád stack előre definiálásával.

Modellezzünk egy fűnyírót monádok segítségével! Azt szeretnénk ha a monád az alábbiakra lenne képes:
- Legyen egy `[(Int, Int)]` típusú írási környezete (mely mezőket nyírta le a fűnyíró)
- Legyen egy `(Int, Int) -> Bool` típusú állapotváltozási környezete (egy adott koordinátájú mezőn van-e fű)
- Legyen `IO` képes környezete

__(3 pont)__

Az alábbi műveletet használjuk a tesztek során.
```hs
runLawnmover lawnmoverMonad initialState = runWriterT $ runStateT lawnmoverMonad initialState
```

Definiálj egy `mowAt` függvényt, amely az adott koordinátán levágja a füvet, ha van. Ha van, ezt az írási környezetbe írjuk ki. __(3 pont)__
Tesztek (ezek nem egyenlőségvizsgálatok, hanem konzolra kiírt kimenetek):
```hs
runLawnmover (mowAt (0,0)) (const True) >>= \((_, f), w) -> print w >> print (f (0,0)) >> print (f (1,1))
====
[(0,0)]
False
True

runLawnmover (mowAt (0,0) >> mowAt (1,1) >> mowAt (0,0)) (const True) >>= \((_, f), w) -> print w >> print (f (0,0)) >> print (f (1,1)) >> print (f (1,2))
====
[(0,0),(1,1)]
False
False
True
```

Definiálj egy `drawLawn` függvényt, amely a paraméterül vár két koordinátát, majd a konzolra kirajzolja a két koordináta által bezárt gyepet. A levágott füvet `#`-el jelöljük, a nem levágott füvet `*`-al. __(4 pont)__

Tesztek (ezek nem egyenlőségvizsgálatok, hanem konzolra kiírt kimenetek):
```hs
void $ runLawnmover (mowAt (0,0) >> drawLawn (0,0) (2,2)) (const True)
====
# * * 
* * * 
* * *

void $ runLawnmover (mowAt (0,0) >> drawLawn (negate 1,negate 3) (2,2)) (const True)
====
* * * * * * 
* * * # * * 
* * * * * * 
* * * * * *

void $ runLawnmover (mowAt (0,0) >> mowAt (2,1) >> drawLawn (negate 1,negate 1) (2,2) >> mowAt (1,1)) (const True)
====
* * * * 
* # * * 
* * * * 
* * # * 
```


## Parser Interpreter feladatok (10 pont)

A feladat a `while` nyelv kiegészítése listákkal.

Adjuk a szintaxishoz az `(:!!) :: Exp -> Exp -> Exp`, `ListLit :: [Exp] -> Exp` és `AssignAt :: String -> Exp -> Exp -> Statement` konstruktorokat.

### Parser

A szintaxis a következő:

- A `(:!!)` egy balra asszociáló operátor legyen, melynek a kötési erőssége 16. Az operátorszimbólum legyen `!!`.
- A `ListLit` konstruktor reprezentáljon lista literálokat (amelyeket az atomok szintjén parseoljunk). Egy list literál `[]`-en belül legyen (0 vagy több elem) és a kifejezések legyenek `,`-vel elválasztva. Minden elem kifejezésben a precedencia szint kezdődjön előröl!
- Az `AssignAt` állítás szintaxisa egyezen meg az értékadáséval, kivéve, hogy az értékadás bal oldalán egy változónév legyen, utána egy `!!` szimbólum, majd egy kifejezés, ami az indexet reprezentálja.

__(5 pont)__

Tesztek a működésre:
```hs
runParser pExp "1 + [2,3   , 4]"  == Right (1 + ListLit [2,3,4],"")
runParser pExp "[] !! 3" == Right (ListLit [] :!! 3,"")
runParser pExp "[                      [       [ ] ]  ]" == Right (ListLit [ListLit [ListLit []]],"")
runParser pExp "[1 + 2, 3 + 4 !! 2 * 1]" == Right (ListLit [1 + 2, 3 + (4 :!! (2 * 1))],"")
runParser program "x !! 1 := 2;" == Right ([AssignAt "x" 1 2],"")
runParser program "x !! 1 := x !! 2" == Just ([AssignAt "x" (IntLit 1) (Index (Var "x") (IntLit 2))],"")
runParser program "x := [1,2,3]; x !! 2 := lam x -> x; x := x !! 2 $ 1;" == [Assign "x" (ListLit [1,2,3]), AssignAt "x" 2 (LamLit "x" (Var "x")), Assign "x" ((Var "x" :!! 2) :$ 1)]
```

### Interpreter

Egészítsd ki a `Val` típust egy `VList :: [Val] -> Val` konstruktorral és a kivételek típusát egy `IndexOutOfRangeError :: Int -> [Val] -> InterpreterError` konstruktorral.

Az új műveletek működése a következő:

- A `List` literál értékelje ki a benne tárolt összes kifejezést és adja őket egy `VList`-ben vissza.
- Az `Index` művelet csak akkor helyes, ha az első paraméter egy `VList`-re, a második pedig egy `VInt`-re értékelődik ki és a benne tárolt érték kisebb mint a lista hossza. Egyéb esetben dobjunk `IndexOutOfRangeError` kivételt.
- Ha a típusok stimmelnek, akkor az első paraméter értékébe indexeljünk bele a második paraméter értékével.
- Az `AssignAt` művelet első paramétere egy változónevet reprezentáljon, ami egy `VList`-re értékelődik majd ki, a második paramétere pedig egy `VInt`-re. Ha a típusok stimmelnek, akkor írjuk fölül az adott változó i-edik elemét a harmadik paraméterrel kiértékelt értékre. Ha szám kilóg a lista méretéből, akkor dobjunk `IndexOutOfRangeError` kivételt.
- Az indexelés mindkét műveletnél 0-tól kezdődik.
- Listák közti összeadás esetén a két listát konkatenáljuk. 
- A lista literálok minden más művelettel típushibát dobnak (kivéve az egyenlőségnél, ott két lista akkor egyenlő, ha az összes elemük és a hosszuk egyenlő. Egyenlőséget elég a `Val` típus `Eq` instanceával vizsgálni).

__(5 pont)__

Tesztek a működésre:
```hs
runProgram [Assign "x" 1, Assign "y" (ListLit [1,2,3,4,5])] == Right [("x",VInt 1),("y",VList [VInt 1,VInt 2,VInt 3,VInt 4,VInt 5])]
runProgram [Assign "x" (ListLit [1,2,3]),AssignAt "x" 2 (LamLit "x" (Var "x")),Assign "x" ((Var "x" :!! 2) :$ 1)] == Right [("x",VInt 1)]
runProgram [Assign "x" (ListLit [1,2]), Assign "y" (ListLit [3, Var "x"]), If (Var "x" :== (Var "y" :!! 1)) [Assign "x" (ListLit [1,2,3])]] == Right [("x",VList [VInt 1,VInt 2,VInt 3]),("y",VList [VInt 3,VList [VInt 1,VInt 2]])]
runProgram [Assign "x" 0, Assign "y" (ListLit [1,2,3,4,5]), While (Not (Var "x" :== 5)) [AssignAt "y" (Var "x") ((Var "y" :!! Var "x") + Var "x"), Assign "x" (Var "x" + 1)]] == Right [("x",VInt 5),("y",VList [VInt 1,VInt 3,VInt 5,VInt 7,VInt 9])]
runProgram [Assign "x" (ListLit [1]), While (Not (Var "x" :!! 0 :== 10)) [Assign "x" (ListLit [1 + (Var "x" :!! 0)] + Var "x")]] == Right [("x",VList [VInt 10,VInt 9,VInt 8,VInt 7,VInt 6,VInt 5,VInt 4,VInt 3,VInt 2,VInt 1])]
runProgram [Assign "x" (ListLit [] :!! 1)] == Left (IndexOutOfRangeError 1 [])
runProgram [Assign "x" (ListLit [101] :!! 1)] == Left (IndexOutOfRangeError 1 [VInt 101])
runProgram [Assign "x" (ListLit [LamLit "x" (Var "x")]), AssignAt "x" 1 1] == Left (IndexOutOfRangeError 1 [VLam "x" [] (Var "x")])
```
