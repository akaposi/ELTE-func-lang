module While where 

import Control.Applicative
import Practice09

-- abstract syntax tree ~ AST

-- "1 + 2"       -> Plus (I 1) (I 2)
-- "1 + (2 + 3)" -> Plus (I 1) (Plus (I 2) (I 3))

data Literal = LBool Bool | LInt Int 
  deriving (Eq, Ord, Show) 
  
data Expr 
  = ELit  Literal
  | EVar  Var
  | EPlus Expr Expr 
  | EAnd  Expr Expr
  deriving (Eq, Ord, Show)

-- "(1 + 2) + 3"
expr1 :: Expr 
expr1 = EPlus (EPlus (ELit (LInt 1)) (ELit (LInt 2))) (ELit (LInt 3))

bLit :: Parser Literal 
bLit = token "true"  *> pure (LBool True)
   <|> token "false" *> pure (LBool False)

iLit :: Parser Literal 
iLit = LInt <$> (natural <* ws)
-- iLit = `fmap` LInt natural
-- iLit = LInt `fmap` natural

lit :: Parser Literal 
lit = bLit <|> iLit

exprLit :: Parser Expr 
exprLit = ELit <$> lit

expr :: Parser Expr 
expr = EPlus <$> exprLit <*> (token "+"  *> expr)
   <|> EAnd  <$> exprLit <*> (token "&&" *> expr)
   <|> exprLit

data Var = Var String 
  deriving (Eq, Ord, Show)

data Statement
  = Skip 
  | Assign Var Expr 
  | Seq Statement Statement 
  | IfThenElse Expr Statement Statement 
  | While Expr Statement 
  deriving (Eq, Ord, Show)  

var :: Parser Var 
var = Var <$> many lowerAlpha <* ws

statement :: Parser Statement 
statement = token "skip" *> pure Skip <|>
            Assign <$> var <*> (token ":=" *> expr) <|>
            IfThenElse <$> (token "If" *> expr) 
                       <*> (token "then" *> statement) 
                       <*> (token "else" *> statement)

runParser intTuple "12" == Just ((1,2), "")
runParser intTuple "12345" == Just ((1,2), "345")
runParser intTuple "" == Nothing
runParser intTuple "1" == Nothing

"" -> [] és nem maradt semmi
"0" -> [0] és nem maradt semmi
"1" -> [1] és nem maradt semmi
"0110" -> [0,1,1,0] és nem maradt semmi
"012" -> fail

runParser bits "" = Just ([], "")
runParser bits "0" = Just ([0], "")
runParser bits "1" = Just ([1], "")
runParser bits "0110" = Just ([0,1,1,0], "")
runParser bits "012" = Nothing