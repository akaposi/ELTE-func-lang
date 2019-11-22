{-# LANGUAGE InstanceSigs #-}
module Practice09 where


-- transactional
newtype Parser a = P { runParser :: String -> Maybe (a, String) }
-- the State is always calculated
-- ~ State String (Maybe a)
-- newtype Parser a = P { runParser :: String -> (Maybe a, String) } 

instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = undefined

instance Applicative Parser where 
  pure :: a -> Parser a 
  pure = undefined

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = undefined

instance Monad Parser where 
  return :: a -> Parser a 
  return = pure 

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = undefined