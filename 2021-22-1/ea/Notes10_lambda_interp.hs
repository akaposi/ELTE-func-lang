{-# language OverloadedStrings #-}

import Data.String

-- típusozatlan lambda-kifejezések kiértékelése (extra anyag)
--------------------------------------------------------------------------------

data Exp =
    Var String          --
  | Lam String Exp      -- \x -> e
  | App Exp Exp         -- e1 e2
  | Let String Exp Exp  -- let x = e1 in e2
  | IntLit Int
  | Add Exp Exp         -- e1 + e2
  deriving Show

instance IsString Exp where
  fromString str = Var str

instance Num Exp where
  fromInteger n = IntLit (fromIntegral n)
  (+)    = Add
  (*)    = undefined
  abs    = undefined
  signum = undefined
  negate = undefined

type Env = [(String, Val)]

data Val
  = VInt Int
  | VLam String Env Exp     -- "closure" : környezet ("capture") + fv kód
  deriving Show

eval :: Env -> Exp -> Val
eval env exp = case exp of

  Var x -> case lookup x env of
    Nothing -> error $ "name not in scope: " ++ x
    Just v  -> v

  IntLit n -> VInt n

  Add e1 e2 -> case (eval env e1, eval env e2) of
    (VInt n1, VInt n2) -> VInt (n1 + n2)
    _                  -> error "type error"

  Let x e1 e2 -> let v = eval env e1 in eval ((x, v):env) e2

  Lam x e -> VLam x env e

  App e1 e2 -> case eval env e1 of
    VLam x env' e1' -> eval ((x, eval env e2):env') e1'
    _               -> error "type error"

--------------------------------------------------------------------------------

($$) :: Exp -> Exp -> Exp
($$) = App
infixl 8 $$

p1 :: Exp
p1 =
  Let "id" (Lam "x" "x") $

  Let "add10" (Lam "x" $ "x" + 10) $

  Let "threeTimes" (Lam "f" $ Lam "x" $ "f" $$ ("f" $$ ("f" $$ "x"))) $

  "threeTimes" $$ ("id" $$ "add10") $$ 0

p2 :: Exp
p2 =
  Let "x" 100 $
  Let "f" (Lam "y" $ "y" + "x") $
  "f" $$ 30

  -- apply (VLam "y" [("x",VInt 100)] (Add (Var "y") (Var "x")))  (VInt 30)
  --   --> eval [("y", VInt 30), ("x",VInt 100)] (Add (Var "y") (Var "x"))
  --   --> VInt 130

run :: Exp -> Val
run = eval []

-- zárt kiértékelés:    minden változónak van értéke
-- nyílt kiértékelés:  lehetnek "szabad változók", változók érték nélkül

-- "normalizálás": Exp -> Exp

-- (\x -> x + 20 + 20) --> (\x -> x + 40)
-- (\x -> id (id (x + 10))) --> (\x -> x + 10)

-- "normalization by evaluation"
--    - program optimalizálás, típuselleőrzés, program-elemzés, stb.
--    - széleskörű felhasználások
