module Machine where
import Control.Monad.Trans.State (State, get, put, runState)

data Drink = Cappuccino | HotChocolatte | Latte | Moccha | Tea
  deriving (Eq, Show)

type Balance = Int
type Cost = Int
type AvailableAmount = Int
newtype Machine = Machine (Balance, [(Drink, Cost, AvailableAmount)])
  deriving (Eq, Show)

tossACoinToYour :: Int -> State Machine ()
tossACoinToYour i = do
  Machine (b , xs) <- get
  put $ Machine (b + i, xs)

refill :: [(Drink, Cost, AvailableAmount)] -> State Machine ()
refill [] = return ()
refill ((drink, cost, amount) : xs) = do
  Machine (b, ys) <- get
  let k = filter (\(d, _, _) -> d /= drink) ys
  let dr = foldr (\(_, cos, am) (dri, c, a) -> (dri, max cos c, a + am)) (drink, cost, amount) $ filter (\(d, _, _) -> d == drink) ys
  put $ Machine (b, dr : k) 
  refill xs

buy :: Drink -> State Machine Bool
buy drink = do
  Machine (balance, ys) <- get
  let k = filter (\(d, _, _) -> d /= drink) ys
  let (d, cost, amount) = foldr (\(_, cos, am) (dri, c, a) -> (dri, max cos c, a + am)) (drink, 0, 0) $ filter (\(d, _, _) -> d == drink) ys
  if cost <= balance && amount > 0 then do
    put $ Machine (balance - cost, (d, cost, amount - 1) : k)
    return True
  else
    return False

init :: Machine
init = Machine (0, [])

{-
Árak:
Tea           - 250
Cappuccino    - 300
HotChocolatte - 275
Latte         - 325
Moccha        - 350
-}

{-
Feladat:
valósítsuk meg a `shopping :: State Machine Int` függvényt!
Töltsük fel a gépet 2 Teával, 0 Latte-vel, 1 Moccha-val, 15 HotChocolatte-al, és 20 Cappuccino-val!
Ezek után vegyünk 3 Teát!
Adjunk bele 5000 ft-t értékű pénzt!
Ezek után vegyünk 1 Teát, 1 Moccha-t!
Majd töltsük fel a gépet 8 Latte-vel!
Végül vegyünk még 2 Teát!
Végeredményben adjuk meg, hány darab vásárlás volt sikeres?

-}

shopping :: State Machine Int
shopping = do
  refill [(Tea, 250, 2), (Latte, 325, 0), (Moccha, 350, 1), (HotChocolatte, 275, 15), (Cappuccino, 300, 20)]
  x0 <- buy Tea
  x1 <- buy Tea
  x2 <- buy Tea
  tossACoinToYour 5000
  x3 <- buy Tea
  x4 <- buy Moccha
  refill [(Latte, 325, 8)]
  x5 <- buy Tea
  x6 <- buy Tea
  return $ length $ filter id [x0, x1, x2, x3, x4, x5, x6]

{- >>> runState shopping Machine.init 
(3,Machine (4150,[(Tea,250,0),(Latte,325,8),(Moccha,350,0),(Cappuccino,300,20),(HotChocolatte,275,15)]))
-}
