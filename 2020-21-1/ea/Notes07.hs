

-- State interpreter példa, Parser monád
--------------------------------------------------------------------------------

-- 1. házi: 4 pont, határidő szorgalmi időszak vége (opcionális) (utolsó beküldés számít)
--          State interpreter feladat (While)

--------------------------------------------------------------------------------

{-# language OverloadedStrings #-}
{-# language DeriveFunctor #-}

import Data.Traversable
import Control.Applicative
import Control.Monad
import Data.String

newtype State s a = State {runState :: s -> (a, s)} deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  State f >>= g = State (\s -> case f s of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------

-- utasítás nyelv: két típus: Bool, Int
--     kifejezések: változók, aritmetika, logikai művelet
--     utasítások: értékadás

type Name = String   -- változónév

data Exp
  = Add Exp Exp    -- e1 + e2
  | Mul Exp Exp    -- e1 * e2
  | Lt  Exp Exp    -- e1 < e2
  | Not Exp        -- not e1
  | And Exp Exp    -- e1 && e2

  | IntLit Int     -- szám literál
  | BoolLit Bool   -- Bool literál
  | Var Name       -- változó
  deriving (Show)

-- példa:

e1 :: Exp
e1 = Add (Mul (Var "x") (Var "y")) (IntLit 10)  -- (x * y) + 10

e2 :: Exp
e2 = Add (Var "x") (BoolLit False)

-- kiértékelés: szükség van változók értékeire (környezet)
type Val = Either Int Bool   -- minden érték Int vagy Bool
type Env = [(Name, Val)]     -- minden névhez egy értéket rendel


-- -- tiszta kiértékelés (read-only Env)
-- evalExp :: Env -> Exp -> Val


-- (mi Exp-ünk nem jár mellékhatással, de gyakorlás kedvéért definiáljuk a State
-- kiértékelést)

-- kiértékelés + lehetséges mellékhatás (Env módosulhat)
evalExp :: Exp -> State Env Val
evalExp (Add e1 e2) = do
  v1 <- evalExp e1  -- v1 :: Val
  v2 <- evalExp e2  -- v2 :: Val
  case (v1, v2) of
    (Left n1, Left n2) -> pure $ Left (n1 + n2)
    _                  -> undefined
evalExp (Mul e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (Left n1, Left n2) -> pure $ Left (n1 * n2)
    _                  -> undefined
evalExp (Lt  e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (Left n1, Left n2) -> pure $ Right (n1 < n2)
    _                  -> undefined
evalExp (And e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (Right b1, Right b2) -> pure $ Right (b1 && b2)
    _                    -> undefined
evalExp (Not e) = do
  v <- evalExp e
  case v of
    Right b -> pure $ Right (not b)
    _       -> undefined

evalExp (IntLit n)  = pure $ Left n
evalExp (BoolLit b) = pure $ Right b

evalExp (Var x) = do
  env <- get
  case lookup x env of
    Just v  -> pure v
    Nothing -> undefined -- változó nincs a környezetben

-- példák:

-- evalState (evalExp e1) [("x", Left 20), ("y", Left 30)] == Left 610
-- evalState (evalExp e1) [("x", Right False), ("y", Left 30)]  -- hiba


-- túlterhelések használata tömörebb Exp-hez
--------------------------------------------------------------------------------

-- 1. Num instance Exp-re (szám literálok típusa :: Num a => a)
-- (Szám literál, (+), (*))

instance Num Exp where
  fromInteger n = IntLit (fromIntegral n)
  (+) = Add
  (*) = Mul
  abs = undefined
  signum = undefined
  negate = undefined

-- példa:
e1' :: Exp
e1' = Var "x" * Var "y" + 10

-- 2. String literálok túlterhelése változókra
-- import Data.String
-- {-# language OverloadedStrings #-}
-- instance IsString Exp

instance IsString Exp where
  fromString = Var

-- példa:
e1'' :: Exp
e1'' = "x" * "y" + 10


-- utasítások
--------------------------------------------------------------------------------

-- cukorka: hozzárendelés konstruktor legyen egy operátor
--          minden data konstruktor lehet operátor, viszont :-al kell kezdődnie
data Statement = (:=) Name Exp   -- (:=) :: Name -> Exp -> Statement
infix 3 :=

type Program = [Statement]

prog1 :: Program
prog1 = [
  "x" := 100,
  "x" := "x" + 10,
  "y" := "x" * "x",
  "z" := 100,
  "b" := Lt "x" "y"
  ]

-- Env kiegészítés + értékadás:
--    ha x szerepel, akkor értékét módosítjuk
--    egyébként a lista végére felvesszük az új értékkel

updEnv :: Name -> Val -> Env -> Env
updEnv x v [] = [(x, v)]
updEnv x v ((x', v'):env)
  | x == x'   = (x', v):env
  | otherwise = (x', v') : updEnv x v env

evalStatement :: Statement -> State Env ()   -- csak a hatás érdekes
evalStatement (x := e) = do
  v <- evalExp e
  modify (updEnv x v)   -- updEnv x v :: Env -> Env

evalProgram :: Program -> State Env ()
evalProgram prog = mapM_ evalStatement prog

-- segédfüggvény:
runProgram :: Program -> Env -> Env
runProgram prog env = execState (evalProgram prog) env

-- runProgram prog1 [] == [("x",Left 110),("y",Left 12100),("z",Left 100),("b",Right True)]
-- házi feladat: szükség van rekurzióra (While, If állítás), Statement is fa-szerű az If/While test miatt


-- Parser monád bevezetés
--------------------------------------------------------------------------------

-- mi is egy parser függvény?
-- String-ből csinál "a"-t

-- String -> a
-- String -> Maybe a               -- + hibalehetőség
-- String -> Maybe (a, String)     -- + maradék String sikeres olvasás esetén

-- Maybe + State monád kombinálása
--  1. hiba propagálása
--  2. input propagálása

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> case g s of
    Nothing     -> Nothing
    Just (a, s) -> Just (f a, s)

  -- fmap f (Parser g) = Parser $ fmap (fmap (\(a, s) -> (f a, s))) g
  --    (Parser fmap: függvény + Maybe fmap)

  -- (nem anyag: Parser = StateT String Maybe    (monád transzformer: egymásra teszünk több monádot)
  -- (monad transformers)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  -- sikeresen visszadjuk a-t és nem olvasunk inputot
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s -> case f s of
    Nothing      -> Nothing
    Just (a, s') -> runParser (g a) s'

-- Parser kombinátor library : rekurzív leszálló parser-ek
-- deklaratív jelleg, kis parserek kombinálása nagy parser-ekké
-- (deklaratív: parser hasonló valamilyen nyelvtan leírásához)

-- elemi Parser-ek
--------------------------------------------------------------------------------

-- üres input olvasása
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

-- feltételt kielégítő karakter olvasása:
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs -> if f c then Just (c, cs) else Nothing
  []   -> Nothing

-- bármilyen karakter olvasása
anyChar :: Parser Char
anyChar = satisfy (\_ -> True)

-- pontosan három hosszú String olvasása
p1 :: Parser ()
p1 = anyChar >> anyChar >> anyChar >> eof

-- konkrét karakter olvasása
char :: Char -> Parser ()
char c = () <$ satisfy (==c)    -- kicseréljük ()-ra a végeredmény Char-t

-- konkrét String olvasása
string :: String -> Parser ()      -- type String = [Char]
string str = mapM_ char str        -- olvassuk az *összes* Char-t a listában egymás után

-- runParser (string "foobar") "foobarxxxxxxxxxxx" == Just ((),"xxxxxxxxxxx")
-- runParser (string "foobar") "xfoobarxxxxxxxxxxx" == Nothing

-- egyelőre milyen regexp-et tudunk reprodukálni:
--   input vége:     $ = eof
--   üres string:    ε = pure ()
--   literál:        c = char c
--   konkatenáció:   e₁e₂ = e₁ >> e₂
--   választás?      e₁|e₂ = ?
--   iteráció?       e* = ?

-- (nem csak regexp-eket tudunk majd írni, hanem Turing-teljes parsert)
