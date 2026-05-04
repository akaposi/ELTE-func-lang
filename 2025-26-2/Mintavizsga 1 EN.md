# Functional Languages Sample Exam

You have 2.5 hours to complete the exam. External assistance and collaboration are not permitted (this includes collaborating with other examinees, external individuals, or artificial intelligence such as ChatGPT, BingAI, etc.). You may submit your solution as many times as you like; the last submission will be the one that counts. Partial credit is available for each problem. At the end of the description, you will find the complete Haskell file, which you must complete and upload.

If you have earned 28 points from the practical assessments, we will automatically award you the +1 credit provided the exam score meets the passing threshold.


# Score Ranges

  - __2__: 15 - 17
  - __3__: 18 - 21
  - __4__: 22 - 25
  - __5__: 26 - 30

## Data-related tasks (10 points)

Given the following type:
```hs
data LengthIndexedList i a = Nil | Cons i a (LengthIndexedList i a) deriving (Eq, Show)
```

Write `Functor`, `Foldable`, and `Traversable` instances for the above type (that can be generated algorithmically)! __(2 = 0.5 + 0.5 + 1 point)__

`deriving` cannot be used in any form.

Examples used in the tests:
```hs
l1 :: LengthIndexedList Integer Char
l1 = Cons 11 'h' $ Cons 10 'e' $ Cons 9 'l' $ Cons 8 'l' $ Cons 7 'o' $ Cons 6 ' ' $ Cons 5 'w' $ Cons 4 'o' $ Cons 3 'r' $ Cons 2 'l' $ Cons 1 'd' $ Nil

l2 :: LengthIndexedList Int Bool
l2 = let l@(Cons i a r) = fmap not l2 in Cons (i + 1) True l

l3 :: LengthIndexedList Integer Int
l3 = Cons 2 2 $ Cons 1 1 Nil

```

Functionality tests:
```hs
sum l3 == 3
not $ and l2
toList l1 == "hello world"
fmap (+1) l3 == Cons 2 3 (Cons 1 2 Nil)
traverse (\a -> if a > 2 then Just (a ^ 2) else Nothing) l3 == Nothing
traverse (\a -> if a <= 2 then Just (a ^ 2) else Nothing) l3 == Just (Cons 2 4 (Cons 1 1 Nil))
```

The `i`-type parameter of the list we defined represents the current length of the list. Define a `satisfyInvariant` function that checks whether this statement is true. __(2 points)__

```hs
satisfyInvariant :: (Num i, Eq i) => LengthIndexedList i a -> Bool
```

Tests to verify functionality:
```hs
satisfyInvariant l1
satisfyInvariant l3
not $ satisfyInvariant $ Cons 2 'a' Nil
not $ satisfyInvariant $ Cons 3 'b' $ Cons 1 'a' Nil
```

Define a `mkLIL` function that generates an LIL satisfying the above invariant from a foldable structure! __(3 points)__

```hs
mkLIL :: (Foldable f, Num i) => f a -> LengthIndexedList i a
```

Tests to verify functionality:
```hs
mkLIL "hello world" == l1
mkLIL [1,2,3] == Cons 3 1 (Cons 2 2 (Cons 1 3 Nil))
mkLIL Nothing == Nil
mkLIL "apple" == Cons 4 'a' (Cons 3 'l' (Cons 2 'm' (Cons 1 'a' Nil)))
```

Define a `reverseLIL` function that reverses a LIL! __(3 points)__
```hs
reverseLIL :: Num i => LengthIndexedList i a -> LengthIndexedList i a
```

Tests:
```hs
reverseLIL (mkLIL "hello") == Cons 5 'o' (Cons 4 'l' (Cons 3 'l' (Cons 2 'e' (Cons 1 'h' Nil))))
reverseLIL (mkLIL [1,2]) == l3
```

## Monad Transformer Exercises (10 points)

The monad transformers used in these exercises can be constructed using type classes or by predefining the entire monad stack.

Let’s model a lawn mower using monads! We want the monad to be capable of the following:
- It should have a writer environment of type `[(Int, Int)]` (which tiles the lawnmower has mowed)
- It should have a state environment of type `(Int, Int) -> Bool` (whether there is grass on a tile at a given coordinate)
- It should have an `IO`-capable environment

__(3 points)__

We will use the following operation during testing.
```hs
runLawnmover lawnmoverMonad initialState = runWriterT $ runStateT lawnmoverMonad initialState
```

Define a `mowAt` function that mows the grass at the given coordinates, if there is any. If there is, write this into the writer environment. __(3 points)__
Tests (these are not equality checks, but console outputs):
```hs
runLawnmover (mowAt (0,0)) (const True) >>= \((_, f), w) -> print w >> print (f (0,0)) >> print (f (1,1))
====
[(0,0)]
False
True

runLawnmover (mowAt (0,0) >> mowAt (1,1) >> mowAt (0,0)) (const True) >>= \((_, f), w) -> print w >> print (f (0,0)) >> print (f (1,1)) >> print (f (1,2))
====
[(0,0),(1,1)]
False
False
True
```

Define a `drawLawn` function that takes two coordinates as parameters and then draws the lawn enclosed by those two coordinates to the console. Mark the mowed grass with `#` and the unmowed grass with `*`. __(4 points)__

Tests (these are not equality checks, but console outputs):
```hs
void $ runLawnmover (mowAt (0,0) >> drawLawn (0,0) (2,2)) (const True)
====
# * * 
* * * 
* * *

void $ runLawnmover (mowAt (0,0) >> drawLawn (negate 1,negate 3) (2,2)) (const True)
====
* * * * * * 
* * * # * * 
* * * * * * 
* * * * * *

void $ runLawnmover (mowAt (0,0) >> mowAt (2,1) >> drawLawn (negate 1,negate 1) (2,2) >> mowAt (1,1)) (const True)
====
* * * * 
* # * * 
* * * * 
* * # * 
```


## Parser and Interpreter Tasks (10 points)

The task is to extend the `while` language with lists.

Add the following constructors to the syntax: `(:!!) :: Exp -> Exp -> Exp`, `ListLit :: [Exp] -> Exp`, and `AssignAt :: String -> Exp -> Exp -> Statement`.

### Parser

The syntax is as follows:

- `(:!!)` should be a left-associative operator with a precedence of 16. The operator symbol should be `!!`.
- The `ListLit` constructor should represent list literals (which we parse at the atom level). A list literal should be enclosed in `[]` (containing 0 or more elements), and the expressions should be separated by `,`. Don't forget to reset the precedence during the parsing of the list elements!
- The syntax of the `AssignAt` statement is identical to that of assignment, except that the left side of the assignment consists of a variable name, followed by a `!!` symbol, and then an expression representing the index.
__(5 pont)__

Functionality tests:
```hs
runParser pExp "1 + [2,3   , 4]"  == Right (1 + ListLit [2,3,4],"")
runParser pExp "[] !! 3" == Right (ListLit [] :!! 3,"")
runParser pExp "[                      [       [ ] ]  ]" == Right (ListLit [ListLit [ListLit []]],"")
runParser pExp "[1 + 2, 3 + 4 !! 2 * 1]" == Right (ListLit [1 + 2, 3 + (4 :!! (2 * 1))],"")
runParser program "x !! 1 := 2;" == Right ([AssignAt "x" 1 2],"")
runParser program "x !! 1 := x !! 2" == Just ([AssignAt "x" (IntLit 1) (Index (Var "x") (IntLit 2))],"")
runParser program "x := [1,2,3]; x !! 2 := lam x -> x; x := x !! 2 $ 1;" == [Assign "x" (ListLit [1,2,3]), AssignAt "x" 2 (LamLit "x" (Var "x")), Assign "x" ((Var "x" :!! 2) :$ 1)]
```

### Interpreter

Extend the `Val` type with a `VList :: [Val] -> Val` constructor and the exception type with an `IndexOutOfRangeError :: Int -> [Val] -> InterpreterError` constructor.

The new operations work as follows:

- The `List` literal should evaluate all expressions stored within it and return them in a `VList`.
- The `Index` operation is valid only if the first parameter evaluates to a `VList` and the second to a `VInt` and the stored value is less than the length of the list. Otherwise, throw an `IndexOutOfRangeError` exception.
- If the types match, insert the value of the second parameter into the value of the first parameter at the specified index.
- The first parameter of the `AssignAt` operation should represent a variable name that evaluates to a `VList`, and the second parameter should evaluate to a `VInt`. If the types match, overwrite the i-th element of the given variable with the value evaluated from the third parameter. If the index exceeds the list’s size, throw an `IndexOutOfRangeError` exception.
- Indexing starts at 0 for both operations.
- In the case of addition where both parameters are lists, concatenate the two lists. 
- List literals raise a type error with all other operations (except for equality, where two lists are equal if all their elements and their lengths are equal. Equality can be checked using the `Eq` instance of the `Val` type).

__(5 points)__

Tests for functionality:
```hs
runProgram [Assign "x" 1, Assign "y" (ListLit [1,2,3,4,5])] == Right [("x",VInt 1),("y",VList [VInt 1,VInt 2,VInt 3,VInt 4,VInt 5])]
runProgram [Assign "x" (ListLit [1,2,3]),AssignAt "x" 2 (LamLit "x" (Var "x")),Assign "x" ((Var "x" :!! 2) :$ 1)] == Right [("x",VInt 1)]
runProgram [Assign "x" (ListLit [1,2]), Assign "y" (ListLit [3, Var "x"]), If (Var "x" :== (Var "y" :!! 1)) [Assign "x" (ListLit [1,2,3])]] == Right [("x",VList [VInt 1,VInt 2,VInt 3]),("y",VList [VInt 3,VList [VInt 1,VInt 2]])]
runProgram [Assign "x" 0, Assign "y" (ListLit [1,2,3,4,5]), While (Not (Var "x" :== 5)) [AssignAt "y" (Var "x") ((Var "y" :!! Var "x") + Var "x"), Assign "x" (Var "x" + 1)]] == Right [("x",VInt 5),("y",VList [VInt 1,VInt 3,VInt 5,VInt 7,VInt 9])]
runProgram [Assign "x" (ListLit [1]), While (Not (Var "x" :!! 0 :== 10)) [Assign "x" (ListLit [1 + (Var "x" :!! 0)] + Var "x")]] == Right [("x",VList [VInt 10,VInt 9,VInt 8,VInt 7,VInt 6,VInt 5,VInt 4,VInt 3,VInt 2,VInt 1])]
runProgram [Assign "x" (ListLit [] :!! 1)] == Left (IndexOutOfRangeError 1 [])
runProgram [Assign "x" (ListLit [101] :!! 1)] == Left (IndexOutOfRangeError 1 [VInt 101])
runProgram [Assign "x" (ListLit [LamLit "x" (Var "x")]), AssignAt "x" 1 1] == Left (IndexOutOfRangeError 1 [VLam "x" [] (Var "x")])
```
