protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get([Char]|S)
    ConsoleGet :: S => Put([Char]|S)
    ConsoleClose :: S => TopBot
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

-- Append two lists
fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

-- Takes two positive ints (a/b), returns their quotient and remainder.
-- Works by repeated subtraction. (New and improved: now with bit-shifts!)
fun div :: Int,Int -> (Int,Int) =
    a,b -> switch
        a < b -> (0,a) -- base case: only remainder left
        b * 2 < a -> case div(a,b+b) of -- Recursive case: double the divisor, and solve.
            (q,r) -> case div(r,b) of
                (q2,r2) -> ((q * 2) + q2, r2)
        True -> case div(a-b,b) of -- Recursive case: repeated subtraction.
            (q,r) -> (q+1,r)

-- Generates a list [...,100,10,1] where the first item is slightly smaller than 'a'
fun tensL :: Int,[Int] -> [Int] =
    a,b:bs -> if a < b*10 then b:bs else tensL(a,(b*10):b:bs)
    _,_ -> [] -- Should never happen


-- int to character
fun itoc :: Int -> Char =
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
    _ -> '?'

-- Takes a positive integer, a list of ints, and returns a string.
-- The list contains [1000,100,10,1] (or something of that format, up to the size of the int.)
fun itosH :: Int,[Int] -> [Char] =
    a,b:bs -> case div(a,b) of
        (q,r) -> itoc(q):itosH(r,bs)
    a,[] -> [] -- The base case.

-- int to string
fun itos :: Int -> [Char] =
    a -> itosH(a,tensL(a,[1]))


fun loop :: Int -> () =
    n -> switch
        0 < n -> loop(n - 1)
        True  -> ()

proc fLoop =
    c | => res -> do
        put loop(c) on res
        halt res

proc pLoop =
    c | => res -> do
        if 0 < c
            then do
                pLoop(c - 1|=>res)
            else do
                put () on res
                halt res
    

proc checker =
    val | a,b,con => -> do
        race
            a -> do -- done
                get _ on a
                get _ on b
                close a
                close b
                hput ConsolePut on con
                put itos(val) on con
                hput ConsolePut on con
                put "done" on con
                hput ConsoleClose on con
                halt con
            b -> do -- try again
                get _ on b
                hput ConsolePut on con
                put "half" on con
                get _ on a
                close a
                close b
                hput ConsolePut on con
                put itos(val) on con
                plug
                    pLoop(1000000|=>a2)
                    fLoop(val + 10000|=>b2)
                    checker(val + 10000|a2,b2,con=>)


proc run =
    | con => -> do
        plug
            pLoop(1000000|=>a)
            fLoop(10000|=>b)
            checker(10000|a,b,con=>)