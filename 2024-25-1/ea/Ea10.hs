{-# LANGUAGE ImplicitParams, ScopedTypeVariables #-}
module Ea10 where

import Control.Monad
import Control.Applicative
import GHC.Stack
import Data.STRef
import Control.Monad.ST

newtype State s a = State (s -> (s,a))

--          v állapotvált  v kezdet      v vég & mellékhatás
runState :: State s a ->   (s ->        (s,a))
runState (State f) = f

instance Functor (State s) where
  -- fmap : (a -> b) -> State s a -> State s b
  -- fmap : (a -> b) -> (s -> (s,a)) -> (s -> (s,b))
  fmap f (State g) = State (\s -> let (s', a) = g s in (s', f a))


instance Applicative (State s) where
  pure a = State (\s -> (s,a))
  (<*>) = ap

instance Monad (State s) where
  -- (>>=) :: (s -> (s,a)) -> (a -> (s -> (s,b))) -> (s -> (s,b))
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State g >>= f = State (\s -> let (s', a) = g s in (let State g' = f a in g' s))


get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> (s, ())


-- Nondet parser

newtype Parser a = Parser { runParser :: String -> [(a, String)] } deriving Functor

instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  (<*>) = ap

instance Monad Parser where
  (Parser rP) >>= f = Parser $ \s -> rP s >>= \(a, s') -> runParser (f a) s'
    -- case rP s of
    -- [] -> []
    -- Just (a, s') -> runParser (f a) s'


instance Alternative Parser where
  empty = Parser $ \_ -> []
  Parser f <|> Parser g = Parser $ \s -> f s ++ g s

many' :: Parser a -> Parser [a]
many' p = some' p <|> pure [] -- Ha 1 vagy több sikertelen, akkor 0 van

some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> many' p -- Lefuttatja 1x és utána 0 vagy többször

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (c:cs) | p c -> [(c, cs)]
  _ -> []

-- Olyan parser, amely akkor fogad el, ha nincs semmi a bemeneten
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> [((), [])]
  _  -> []

-- Ezekből felépíthetőek egyéb parserek
-- Olyan parser, ami egy konkrét karakter ad vissza
-- Itt irreleváns az, hogy milyen karakter parseol
char :: Char -> Parser ()
char c = () <$ satisfy (== c)

-- (c*)i?(c+)a{3}
p3 :: Parser ()
p3 = do
  many (char 'c')
  optional (char 'i')
  some (char 'c')
  replicateM_ 3 (char 'a')


f2 :: (a -> b) -> a -> b
f2 f a = f a

f3 :: (b -> c) -> (a -> b) -> a -> c
f3 = undefined

f4 :: (a -> b -> c) -> b -> a -> c
f4 = undefined


-- HasCallStack

f :: HasCallStack => String
f = show ?callStack

stackTrace :: HasCallStack => String
stackTrace = concatMap
  (\(fun, s) -> "\tcall to '" ++ fun ++ "' at line " ++ show (srcLocStartLine s) ++ ", column " ++ show (srcLocStartCol s) ++ "\n") $
  getCallStack callStack

g :: HasCallStack => Int -> String
g i | i <= 0 = stackTrace
g i = g (i - 1)


plus1 :: forall a. Num a => a -> a
plus1 x = xPlus1
  where
    xPlus1 :: Num a => a
    xPlus1 = x + 1

appId :: Num a => a -> (forall b. b -> b) -> a
appId a f = f 1


raiseTupleToFunctor :: Functor f => (forall c. c -> f c) -> a -> b -> f (a,b)
raiseTupleToFunctor pure a b = pure (a, b)

-- ST
myFunc :: Integer
myFunc = runST $ do
  x <- newSTRef 1
  y <- newSTRef 3
  forM_ [0..10] $ \i -> do
    modifySTRef x (* (i + i))
    modifySTRef y (+ i)
    valY <- readSTRef y
    modifySTRef x (`div` valY)
  readSTRef x
