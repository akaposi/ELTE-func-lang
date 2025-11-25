module Ea9 where

import Control.Monad.State.Strict
import Data.Word
import Data.Char
import Control.Applicative
import Data.Maybe

b1, b2 :: Bool
b1 = False
b2 = True

data Three = T1 | T2 | T3

type Example1 = Either Bool Three

-- Left True
-- Left False
-- Right T1
-- Right T2
-- Right T3

type Example2 = (Bool, Three)

-- (True, T1)
-- (False, T1)
-- (True, T2)
-- ...

-- |Either a b| -- |a| + |b|
-- |(a, b)| -- |a| * |b|

-- |()| = 1

-- () :: ()

data Void
  deriving (Show)

examplex :: Void
examplex = error "bug"

exampley :: Bool
exampley = error "bug"

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

type Example3 = Bool -> Three

ex3 :: Example3
ex3 False = T1 -- 3 possibilities
ex3 True = T2 -- 3
-- 3 * 3 = 3^2

-- |Three -> Bool| = 2^3 = 8

-- |a -> b| = |b|^|a|


type F a = (a, a)

-- f a = a * a
-- (f a)' = a' * a + a * a' = 1 * a + a * 1 = a + a

type F' a = Either a a

-- (_, a) or (a, _)

fillHoleF :: a -> F' a -> F a
fillHoleF x (Left a) = (x, a)
fillHoleF x (Right a) = (a, x)

-- F a = Int
-- F' a = Void

-- F a = a
-- F' a = ()

-- F a = Either a a
-- F' a = Either () ()

data List a = Nil | Cons' a (List a)

-- List a = 1 + a * List a

-- (List a)' = 0 + a' * List a + a * (List a)' = 1 * List a + a * (List a)'
-- (List a)' = List a + a * (List a)'

data List' a = Stop [a] | Cont a (List' a)
-- [1, 2, _, 4, 5, 6]
-- Cont 1 (Cont 2 (Stop [4, 5, 6]))

type List'' a = ([a], [a])

example :: List'' Int
example = ([2, 1], [4, 5, 6])

fillHoleList :: a -> List'' a -> [a]
fillHoleList hole (front, back) = reverse front ++ [hole] ++ back

type Zipper a = ([a], a, [a])

moveLeft :: Zipper a -> Zipper a
moveLeft (xs, y, z:zs) = (y:xs, z, zs)

moveRight :: Zipper a -> Zipper a
moveRight (x:xs, y, zs) = (xs, x, y:zs)

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Tree a = 1 + Tree a * a * Tree a
{-
(Tree a)' =
  (Tree a)' * a * Tree a +
  Tree a * Tree a +
  Tree a * a * (Tree a)'
-}

data Tree' a
  = GoLeft (Tree' a) a (Tree a)
  | StopT (Tree a) (Tree a)
  | GoRight (Tree a) a (Tree' a)

data Step a = StepLeft a (Tree a) | StepRight (Tree a) a
type TreeZipper a = ([Step a], Tree a, a, Tree a)

goUp :: TreeZipper a -> TreeZipper a
goUp (StepLeft sx sr : steps, l, x, r) = (steps, Node l x r, sx, sr)
goUp (StepRight sl sx : steps, l, x, r) = (steps, sl, sx, Node l x r)

goDownLeft :: TreeZipper a -> TreeZipper a
goDownLeft (steps, Node ll lx lr, x, r) = (StepLeft x r : steps, ll, lx, lr)

-- antiderivative
-- ([a], [a]) -> [a]
-- [a] -> Cyclic list

data CyclicZipper a = Cyclic a [a]

cyclicMoveLeft :: CyclicZipper a -> CyclicZipper a
cyclicMoveLeft (Cyclic x xs) = Cyclic (last xs) ([x] ++ init xs)

cyclicMoveRight :: CyclicZipper a -> CyclicZipper a
cyclicMoveRight (Cyclic x xs) = Cyclic (head xs) (tail xs ++ [x])


-- < move left
-- > move right
-- + increment
-- - decrement
-- , input
-- . output
-- [ ] loop if 0

data Instr
  = MoveLeft
  | MoveRight
  | Increment
  | Decrement
  | Input
  | Output
  | Loop Program
  deriving (Show)

type Program = [Instr]

data Stream a = Cons a (Stream a)

repeatS :: a -> Stream a
repeatS x = Cons x (repeatS x)

type Tape a = (Stream a, a, Stream a)

initTape :: a -> Tape a
initTape x = (repeatS x, x, repeatS x)

moveLeftT :: Tape a -> Tape a
moveLeftT (l, x, Cons y r) = (Cons x l, y, r)

moveRightT :: Tape a -> Tape a
moveRightT (Cons x l, y, r) = (l, x, Cons y r)

modifyCurrT :: (a -> a) -> Tape a -> Tape a
modifyCurrT f (l, x, r) = (l, f x, r)

getCurrT :: Tape a -> a
getCurrT (_, x, _) = x

-- Word8 ~ 0..255

interpInstr :: Instr -> StateT (Tape Word8) IO ()
interpInstr instr =case instr of
    MoveLeft -> modify moveLeftT
    MoveRight -> modify moveRightT
    Increment -> modify (modifyCurrT (+ 1))
    Decrement -> modify (modifyCurrT (subtract 1))
    Input -> do
      c <- lift $ getChar
      let b = fromIntegral (ord c)
      modify (modifyCurrT (const b))
    Output -> do
      tape <- get
      let c = chr $ fromIntegral $ getCurrT tape
      lift $ putChar $ chr $ fromIntegral $ getCurrT tape
    Loop program -> do
      let loop = do
            tape <- get
            if getCurrT tape == 0
              then return ()
              else do
                interp program
                loop
      loop

interp :: Program -> StateT (Tape Word8) IO ()
interp [] = return ()
interp (instr : rest) = do
  interpInstr instr
  interp rest

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser p s = runStateT p s

char :: Char -> Parser Char
char c = do
  s <- get
  case s of
    [] -> empty
    c':rest
      | c == c' -> do
        put rest
        return c
      | otherwise -> empty

anyChar :: Parser Char
anyChar = do
  s <- get
  case s of
    [] -> empty
    c:rest -> do
      put rest
      return c

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- get
  case s of
    [] -> empty
    c:rest
      | p c -> do
        put rest
        return c
      | otherwise -> empty

eof :: Parser ()
eof = do
  s <- get
  case s of
    [] -> return ()
    _ -> empty

parseInstr :: Parser Instr
parseInstr =
  MoveLeft <$ char '<' <|>
  MoveRight <$ char '>' <|>
  Increment <$ char '+' <|>
  Decrement <$ char '-' <|>
  Input <$ char ',' <|>
  Output <$ char '.' <|>
  Loop <$ char '[' <*> parseProgram <* char ']'

parseInstrOrComment :: Parser (Maybe Instr)
parseInstrOrComment = Just <$> parseInstr <|> Nothing <$ (satisfy (/= ']'))

parseProgram :: Parser Program
parseProgram = catMaybes <$> many parseInstrOrComment

-- catMaybes = map fromJust . filter isJust

-- examples from Wikipedia
helloWorld :: String
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

rot13 :: String
rot13 =
  unlines
  [ "-,+[                         Read first character and start outer character reading loop"
  , "      -[                       Skip forward if character is 0"
  , "        >>++++[>++++++++<-]  Set up divisor (32) for division loop"
  , "                              (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)"
  , "        <+<-[                Set up dividend (x minus 1) and enter division loop"
  , "            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward"
  , "            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient"
  , "            <<<<<-           Decrement dividend"
  , "        ]                    End division loop"
  , "    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag"
  , "    >--[-[<->+++[-]]]<[         Zero that flag unless quotient was 2 or 3; zero quotient; check flag"
  , "        ++++++++++++<[       If flag then set up divisor (13) for second division loop"
  , "                              (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)"
  , "            >-[>+>>]         Reduce divisor; Normal case: increase remainder"
  , "            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient"
  , "            <<<<<-           Decrease dividend"
  , "        ]                    End division loop"
  , "        >>[<+>-]             Add remainder back to divisor to get a useful 13"
  , "        >[                   Skip forward if quotient was 0"
  , "            -[               Decrement quotient and skip forward if quotient was 1"
  , "                -<<[-]>>     Zero quotient and divisor if quotient was 2"
  , "            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1"
  , "        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0"
  , "    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)"
  , "    <[-]                     Clear remainder from first division if second division was skipped"
  , "    <.[-]                    Output ROT13ed character from copy and clear it"
  , "    <-,+                     Read next character"
  , "]                            End character reading loop"]

main :: IO ()
main = do
  let Just (program, _) = runParser parseProgram rot13
  evalStateT (interp program) (initTape 0)
  print "Hello"
