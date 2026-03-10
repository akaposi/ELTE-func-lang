{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Gyak05 where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

-- Let's implement an access control system in the subway!
-- Let's model this with a state machine:
{-

                 ----------   Ticket / Green  ----------
             /---|        |------------------>|        |---\
 Push / Red |    | Closed |   Push / Green    |  Open  |    | Ticket / Yellow
             \-->|        |<------------------|        |<--/
                 ----------                   ----------

-}

-- In the diagram, the squares symbolize possible states
-- The arrows represent transitions; for example, the arrow going from the 'Closed' state to the 'Open' state means
-- that when we insert a Ticket, it lights up green and we switch to the Open state

-- Let's define the type representing the states of the machine
data MachineState = Open | Closed
  deriving (Eq, Show)

-- Let's define the type representing the colours of the transitions
data LightColour = Red | Yellow | Green
  deriving (Eq, Show)


-- Define the functions representing transitions
-- The functions take a state as an input and return the associated colour and new state
push, insertTicket :: MachineState -> (LightColour, MachineState)
push Open = (Green, Closed)
push Closed = (Red, Closed)

insertTicket Open = (Yellow, Open)
insertTicket Closed = (Green, Open)

-- With this help, we can model, for example, that Pistike tries to enter twice without a ticket.
pistike :: MachineState -> ([LightColour], MachineState)
pistike initialState =
  let
    (l1, s1) = push initialState
    (l2, s2) = push s1
    (l3, s3) = insertTicket s2
    (l4, s4) = push s3
  in ([l1, l2, l3, l4], s4)

-- However, manually handling state changes involves a lot of boilerplate code.
-- In fact, we would prefer not to have to manually pass on s1, s2, s3, and s4.
-- In this form, it is easy to make a mistake (e.g., s2 instead of s3, and it would be as if one line had not been executed).

-- Note that the form of the "state change" process is:
-- s -> (a, s)
-- here s = MachineState and a = LightColour or [LightColour]
-- Functions of the form s -> (a,s) are called State Monads, because for functions of this style
-- you can write the >>= and return operations

{-
newtype State s a = State { runState :: s -> (a,s) }

(>>=) :: m a -> (a -> m b) -> m b 

Composition of state transitions
(>>=) ::  State s a    -> (a -> State s b)  -> State s b
unfolded  (s -> (a,s)) -> (a -> s -> (b,s)) -> s -> (b,s)

return :: a -> m a

No state change
return :: a -> State s a
unfolded  a -> s -> (a,s)

-}

-- Since representation is a little more complicated,
-- s -> (a,s) functions can be packaged with the state function.
pushS, insertTicketS :: State MachineState LightColour
pushS = state push
insertTicketS = state insertTicket

-- Pistike can be defined a bit more elegantly this way
pistikeS :: State MachineState [LightColour]
pistikeS = do             --        |
  l1 <- pushS             --        |
  l2 <- pushS             --        |
  l3 <- insertTicketS     --        |
  l4 <- pushS             --        |
  return [l1, l2, l3, l4] -- <------/ The type dictates the type of the return value

-- or with binds
pistikeS' :: State MachineState [LightColour]
pistikeS' =
  pushS >>= \l1 ->
  pushS >>= \l2 ->
  insertTicketS >>= \l3 ->
  pushS >>= \l4 ->
  return [l1,l2,l3,l4]

-- Use runState

-- You don't always need to return in [LightColour]
countYellow :: State MachineState Int
countYellow = do
  l1 <- insertTicketS
  l2 <- insertTicketS
  l3 <- insertTicketS
  return $ length $ filter (== Yellow) [l1, l2, l3]
  --return 0

-- Tasks
-- Define the order of the following transitions with bindings and do-notation
-- Janika: Ticket, Ticket, Push
-- Gerike: Push, Ticket, Push, Push
-- In Gerike's case, return how many times the result of the transitions was 'Red'

-- do notation
janika :: State MachineState [LightColour]
janika = do
  l1 <- insertTicketS
  l2 <- insertTicketS
  l3 <- pushS
  return [l1, l2, l3]

-- bind notation
janika' :: State MachineState [LightColour]
janika' = 
  insertTicketS >>= \l1 ->
  insertTicketS >>= \l2 ->
  pushS >>= \l3 ->
  return [l1, l2, l3]

gerike :: State MachineState Int
gerike = do
  l1 <- pushS
  l2 <- insertTicketS
  l3 <- pushS
  l4 <- pushS
  return (length (filter (== Red) [l1, l2, l3, l4]))

-- More complex tasks
-- Implement a 'get' operation that returns the state
get' :: State s s -- s -> (s, s)
get' = state (\s -> (s, s))
-- Implement a 'put' operation that overwrites the state
put' :: s -> State s () -- s -> s -> ((), s)
put' s = state (\_ -> ((), s))

-- After this, there is no need to mess around with the 'state' function

-- Example get/put: Define the pop function, which removes the head of the list in the state, if there is one.
pop :: State [a] (Maybe a) -- [a] -> (Maybe a, [a])
pop = do
  l <- get'
  case l of 
    [] -> return Nothing
    (x : xs) -> do
      put' xs
      return (Just x)

-- Example get/put 2: Define the take function for the internal state (pop can also be used).
takeK :: Int -> State [a] [a] -- Int -> [a] -> ([a], [a])
takeK 0 = return []
takeK n = do
  l <- get'
  case l of
    [] -> return []
    (x : xs) -> do
      put' xs
      res <- takeK (n-1) 
      return (x : res)

-- Define the following functions
sumK :: Num a => Int -> State [a] a -- Extracts and adds up the first K elements from the list
sumK n = do
  l <- takeK n
  return (sum l)

popLast :: State [a] (Maybe a) -- removes the last item from the list, if there is one.
popLast = undefined

labelList :: [a] -> State Int [(a, Int)] -- Labels every element with their index, internal state is the counter
labelList = undefined

labelListBW :: [a] -> State Int [(a, Int)] -- Same as the previous function, but counts from the end.
labelListBW = undefined


-- EXCEPT
-- In the previous lesson, we handled errors using the Maybe type, but this type cannot distinguish between different errors
-- Let's use the Either type and a custom error type!
-- The following errors should be possible (all 0-parameter constructors)
-- Division by zero error
-- Assertion error
-- Empty list error
data CustomError = Div0Err | AssertErr | EmptyListErr | OverflowErr
  deriving (Eq, Show)

-- Example: Safe division
safeDiv :: Integral a => a -> a -> Either CustomError a
safeDiv n 0 = Left Div0Err
safeDiv n m = Right (n `div` m)

-- Either is also a monad
bindE :: Either e a -> (a -> Either e b) -> Either e b
bindE (Left e) _ = Left e -- f is never called in the case of an error (errors propagate)
bindE (Right a) f = f a

-- If one can throw errors, one should also be able to catch them
catchE :: Either e a -> (e -> Either e a) -> Either e a
catchE (Left e) f = f e
catchE (Right a) _ = Right a

-- Either has a slightly more general form (more details in a few lessons)
-- This is the Except Monad
-- newtype Except e a = Except { runExcept :: Either e a }

-- primitive functions
-- throwError :: e -> Except e a
-- catchError :: Except e a -> (e -> Except e a) -> Except e a

-- Example of throwError/catchError:
-- Throw an assertion error if the condition is not met
assert :: Bool -> Except CustomError ()
assert True = return ()
assert False = throwError AssertErr

-- Use runExcept

-- Safe division
safeDivE :: Integral a => a -> a -> Except CustomError a
safeDivE n 0 = throwError Div0Err
safeDivE n m = return (n `div` m )

-- Perform the safeDivE operation, but if an exception is thrown, return 0 instead.
safeDivF :: Integral a => a -> a -> Except CustomError a
safeDivF n m = catchError (safeDivE n m) (\err -> return 0)

-- Tasks

-- Let's fold the division operation through a list, and if we need to divide by 0, throw an exception.
foldDiv :: Integral a => [a] -> Except CustomError a
foldDiv = undefined


-- We want to simulate 32-bit numbers, 
-- that is, we want to know if we overflow when adding/multiplying/subtracting
-- we would return with an error
-- Let's add overflow and underflow errors

u32Max :: Integer
u32Max = 2 ^ 32 - 1

(+++) :: Integer -> Integer -> Except CustomError Integer 
(+++) a b = if (x < u32Max) then return x else throwError OverflowErr
  where
    x = a + b

-- Subtraction
(-~-) :: Integer -> Integer -> Except CustomError Integer
(-~-) = undefined

-- Multiplication
(***) :: Integer -> Integer -> Except CustomError Integer
(***) = undefined
