module ParseWhile where

import Control.Applicative

import Syntax
import Parser2

iLit :: Parser Lit
iLit = (LInt <$> natural) <* ws

bLit :: Parser Lit
bLit = token "true"  *> pure (LBool True)
    <|> token "false" *> pure (LBool False)

lit :: Parser Lit
lit = iLit <|> bLit

parens :: Parser a -> Parser a
parens p = token "(" *> p <* token ")"

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = ELit <$> lit
    <|> parens expr

expr :: Parser Expr
expr = Plus <$> expr' <*> (token "+"  *> expr)
    <|> And  <$> expr' <*> (token "&&" *> expr)
    <|> expr'

var :: Parser Var
var = (Var <$> some lowerAlpha) <* ws

statement' :: Parser Statement
statement' = (token "Skip" *> pure Skip)
        <|> Assign <$> var <*> (token ":=" *> expr)
        <|> If <$> (token "If"   *> expr)
                <*> (token "then" *> statement)
                <*> (token "else" *> statement)
        <|> While <$> (token "While" *> expr)
                  <*> (token "do"    *> statement <* token "end")

statement :: Parser Statement
statement = Seq <$> statement' <*> (token ";" *> statement)
        <|> statement'
