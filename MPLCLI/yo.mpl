{-
3
-}

{-
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun fib :: Int -> Int =
    0 -> 0
    1 -> 1
    n -> fib(n - 1) + fib(n - 2)

-- a / b but for integers
fun intDivFloor :: Int, Int -> Int =
    a,b -> if a < b
        then 0
        else 1 + intDivFloor(a - b, b)

fun intRem :: Int, Int -> Int =
    a, b -> if a < b
        then a
        else intRem(a - b, b)

fun intToChar :: Int -> Char =
    0 -> '0'
    1 -> '1'
    2 -> '2'
    3 -> '3'
    4 -> '4'
    5 -> '5'
    6 -> '6'
    7 -> '7'
    8 -> '8'
    9 -> '9'
    n -> '0'

fun intToStr :: Int -> [Char] =
    n -> 
        let fun go =
                v, acc -> if v <= 0
                    then acc
                    else intToChar(intRem(v, 10)) : go(intDivFloor(v, 10), acc)
        in go(n, [])

fun reverse :: [A] -> [A] = 
    lst -> let 
-}

data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

-- less than or equal to for natural numbers. 
fun natLe :: Nat,Nat -> Bool =
    Zero, _ -> True
    Succ(_), Zero -> False
    Succ(a0), Succ(a1) -> natLe(a0,a1)

fun showBool :: Bool -> [Char] =
    True -> "True"
    False -> "False"



coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put showBool(natLe(Succ(Zero),Succ(Zero))) on _console
        hput ConsoleClose on _console
        halt _console

