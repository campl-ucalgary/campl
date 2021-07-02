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



fun append :: [A],[A] -> [A] =
    s:ss, ts -> s : append(ss,ts)
    [], ts -> ts

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun conswrapper :: A,[A] -> [A] =
    s,ss -> s:ss

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put concat(["a","b","c"]) on _console

        hput ConsoleClose on _console
        halt _console

