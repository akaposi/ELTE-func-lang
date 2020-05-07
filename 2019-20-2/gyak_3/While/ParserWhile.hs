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

bLit :: Parser Lit
bLit = LBool <$> bool

iLit :: Parser Lit
iLit = LInt <$> (natural <* ws)

lit :: Parser Lit
lit = bLit <|> iLit

name :: Parser Name
name = some lowerAlpha <* ws

var :: Parser Var
var = Var <$> name

expr' :: Parser Expr
expr' = EVar <$> var
    <|> ELit <$> lit
    <|> token "(" *> expr <* token ")"

expr :: Parser Expr
expr = Plus  <$> (expr' <* token "+") <*> expr
   <|> Minus <$> (expr' <* token "-") <*> expr
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
-}

{-
"1 + 2 + 3" -> 1 + (2 + 3)
(Plus
   (ELit (LInt 1))
   (Plus
      (ELit (LInt 2))
      (ELit (LInt 3)))

"1 - 2 - 3" -> 1 - (2 - 3)
(Minus
   (ELit (LInt 1))
   (Minus
      (ELit (LInt 2))
      (ELit (LInt 3)))

"(1 - 2) - 3" -> (1 - 2) - 3
(Minus
   (Minus
      (ELit (LInt 1))
      (ELit (LInt 2)))
   (ELit (LInt 3))
-}
