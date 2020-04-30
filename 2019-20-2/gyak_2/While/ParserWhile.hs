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

-- TODO: natural
iLit :: Parser Lit
iLit = undefined

lit :: Parser Lit
lit = bLit <|> iLit

-- TODO: lowerAlpha
var :: Parser Var
var = undefined

-- TODO: ELit, EVar ... LEq
expr :: Parser Expr
expr = undefined

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
