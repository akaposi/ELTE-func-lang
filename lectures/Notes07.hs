
-- State, Reader
--------------------------------------------------------------------------------

import Control.Monad.State
import Control.Monad.Reader


-- Ismétlés: kifejezés kiértékelése (lásd Notes03.hs)
------------------------------------------------------------

data Expr = Literal Int | Add Expr Expr | Mul Expr Expr | Var String
  deriving Show

-- "10 + (20 + x)"
e1 = Add (Literal 10) (Add (Literal 20) (Var "x"))

-- "y * x + 100"
e2 = Add (Mul (Var "y") (Var "x")) (Literal 100)

-- Írj kiértékelő függvényt Expr-hez. Paraméterként megkapunk egy '[(String,
-- Int)]' listát, ez minden változónévhez megad egy értéket.

-- példa:
-- eval [("x", 10)] e1 == 40
-- eval [("x", 2), ("y", 2)] e2 == 400

-- példa:
-- eval [("x", 10)] e1 == 40
-- eval [("x", 2), ("y", 2)] e2 == 400

evalExpr :: [(String, Int)] -> Expr -> Int
evalExpr env e = case e of
  Literal n -> n
  Add e1 e2 -> evalExpr env e1 + evalExpr env e2
  Mul e1 e2 -> evalExpr env e1 * evalExpr env e2
  Var x     -> case lookup x env of
                 Just n -> n
                 _      -> error "var not in scope"

-- Egészítsük ki a nyelvet értékadással a következőképpen:
type Program = [(String, Expr)]

-- Egy program értékadó utasítások listája. pl:
p1 = [("x", Literal 10), ("y", Literal 20), ("x", Add (Var "x") (Var "y"))]

-- ez megfelel annak hogy: x := 10; y := 20; x := x + y


-- Írj kiértékelést. Kezdetben a változókörnyezet legyen üres. Ha egy új
-- változóhoz rendel a program értéket, egészítsd ki a környezetet az új
-- változóval és értékkel, egyébként módosítsd meglévő változó értékét.
-- Legyen a "runProgram" visszatérési értéke a környezet végső állapota.

runProgram :: Program -> [(String, Int)]
runProgram prog = execState (evalProgram prog) []

evalProgram :: Program -> State [(String, Int)] ()
evalProgram = mapM_ $ \(x, e) -> do
  env <- get
  let v = evalExpr env e
      update ((x', v'):env)
        | x == x' = (x, v):env
        | otherwise = (x', v'):update env
      update [] = [(x, v)]
  modify update


-- Példák:
-- runProgram [("x", Literal 10)] == [("x", Literal 10)]
-- runProgram [] == []
-- runProgram [("x", Literal 10), ("x", Var "x")] == [("x", 10)]
-- runProgram [("x", Literal 10), ("x", Add (Var "x") (Var "x"))] == [("x", 20)]
-- runProgram [("x", Literal 10), ("y", Var "x")] == [("x", 10), ("y", 10)]


-- Kiértékelés Maybe-vel
--------------------------------------------------------------------------------

-- Értékeld ki a következő kifejezéseket úgy, hogy az eredmény Nothing legyen,
-- ha egy változó nincs a környezetben. Használd a Maybe monádot.

data Exp2 = Var2 String | Add2 Exp2 Exp2 | Mul2 Exp2 Exp2 | Literal2 Int
  deriving Show

evalExp2 :: [(String, Int)] -> Exp2 -> Maybe Int
evalExp2 env e = case e of
  Literal2 n -> pure n
  Add2 e1 e2 -> (+) <$> evalExp2 env e1 <*> evalExp2 env e2
  Mul2 e1 e2 -> (*) <$> evalExp2 env e1 <*> evalExp2 env e2
  Var2 x     -> lookup x env


-- Kiértékelés Reader-el
--------------------------------------------------------------------------------


-- Értékeld ki a következő kifejezéseket, amelyekben let-definíciók is vannak,
-- Reader monád segítségével.

data Exp3 = Var3 String | Add3 Exp3 Exp3 | Mul3 Exp3 Exp3 | Literal3 Int
          | Let String Exp3 Exp3 -- let x = exp1 in exp2
  deriving Show

-- A változókörnyezetet a Reader-ben tároljuk. Használd a "local" függvényt
-- a Let-ek kirétékelésére (a környezet lokális kiegészítéséhez).
evalExp3 :: Exp3 -> Reader [(String, Int)] Int
evalExp3 e = case e of
  Literal3 n  -> pure n
  Add3 e1 e2  -> (+) <$> evalExp3 e1 <*> evalExp3 e2
  Mul3 e1 e2  -> (*) <$> evalExp3 e1 <*> evalExp3 e2
  Var3 x      -> do e <- ask
                    case lookup x e of
                      Just n -> pure n
                      _      -> error "var not in scope"
  Let x e1 e2 -> do v <- evalExp3 e1
                    local ((x, v):) (evalExp3 e2)
