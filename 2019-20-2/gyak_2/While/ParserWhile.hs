module ParserWhile where

import Control.Applicative

import Parser
import Syntax


true :: Parser Bool
true = token "true" *> pure True

false :: Parser Bool
false = token "false" *> pure False

bool :: Parser Bool
bool = true <|> false

-- bool  :: Parser Bool
-- bLit  :: Parser Lit
-- fmap  :: (a -> b) -> f a -> f b
-- fmap  :: (Bool -> Lit) -> Parser Bool -> Parser Lit
-- LBool :: Bool -> Lit

bLit :: Parser Lit
bLit = LBool <$> bool

iLit :: Parser Lit
iLit = LInt <$> (natural <* ws)

lit :: Parser Lit
lit = bLit <|> iLit

var :: Parser Var
var = Var <$> (some lowerAlpha <* ws)

expr' :: Parser Expr
expr' = ELit <$> lit
    <|> EVar <$> var

expr :: Parser Expr
expr = LEq  <$> (expr' <* token "<=") <*> expr
   <|> Plus <$> (expr' <* token "+")  <*> expr
   <|> expr'

statement :: Parser Statement
statement = undefined

program :: Parser Statement
program = statement <* eof


{- tests
runParser iLit "51"   == Just (LInt 51, "")
runParser bLit "true" == Just (LBool True, "")
runParser lit "true"  == Just (LBool True, "")
runParser lit "51"    == Just (LInt 51, "")

runParser var "x"  == Just (Var "x", "")
runParser var "xs" == Just (Var "xs", "")
runParser var "5"  == Nothing
runParser var "x5" == Just (Var "x", "5")

runParser expr "x"  == Just (EVar (Var "x"), "")
runParser expr "53" == Just (ELit (LInt 53), "")

runParser expr "x <= 5" == Just (LEq (EVar (Var "x")) (ELit (LInt 5)),"")
-}

{-
runParser expr "1 + 2 + 3 + 4"

(Plus (ELit (LInt 1))
  (Plus (ELit (LInt 2))
    (Plus (ELit (LInt 3))
          (ELit (LInt 4))))

1 + (2 + (3 + 4))
-}
