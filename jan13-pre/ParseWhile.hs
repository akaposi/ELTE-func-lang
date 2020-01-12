module ParseWhile where

import Control.Applicative

import Syntax
import Parser

iLit :: Parser Lit
iLit = LInt <$> token natural

bLit :: Parser Lit
bLit =  (LBool True  <$ string' "true")
    <|> (LBool False <$ string' "false")

lit :: Parser Lit
lit = iLit <|> bLit

parens :: Parser a -> Parser a
parens p = string' "(" *> p <* string' ")"

keywords :: [String]
keywords = ["Skip", "If", "then", "else", "While", "do", "end", "true", "false", "not"]

ident :: Parser String
ident = do
  x <- token (some lowerAlpha)
  if elem x keywords
    then empty
    else pure x

var :: Parser Var
var = Var <$> ident

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = (ELit <$> lit)
    <|> (EVar <$> var)
    <|> parens expr

expr :: Parser Expr
expr =
        (Not <$> (string' "not" *> expr))
    <|> Plus  <$> expr' <*> (string' "+"  *> expr)
    <|> Minus <$> expr' <*> (string' "-"  *> expr)
    <|> Mul   <$> expr' <*> (string' "*"  *> expr)
    <|> And   <$> expr' <*> (string' "&&" *> expr)
    <|> Eq    <$> expr' <*> (string' "==" *> expr)
    <|> LEq   <$> expr' <*> (string' "<=" *> expr)
    <|> expr'

statement' :: Parser Statement
statement' = (string' "Skip" *> pure Skip)
        <|> Assign <$> var <*> (string' ":=" *> expr)
        <|> If <$> (string' "If"   *> expr)
               <*> (string' "then" *> statement)
               <*> (string' "else" *> statement)
        <|> While <$> (string' "While" *> expr)
                  <*> (string' "do"    *> statement <* string' "end")

statement :: Parser Statement
statement = Seq <$> statement' <*> (string' ";" *> statement)
        <|> statement'
