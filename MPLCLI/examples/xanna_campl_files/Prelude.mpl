
-- This opens a terminal for which one may
-- output strings and get strings on.
protocol StringTerminal => S =
    StringTerminalGet :: Get([Char]| S) => S
    StringTerminalPut :: Put([Char]| S) => S
    StringTerminalClose :: TopBot => S

-- This allows the console to input and output strings, and to
-- open new string terminals.
coprotocol S => Console =
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot
    IntConsolePut :: S => Get(Int| S)
    IntConsoleGet :: S => Put(Int| S)
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

-- This is a timer which waits a given number of microseconds
coprotocol S => Timer =
    -- Timer in microseconds
    Timer :: S => Get(Int | S (*) Put(()| TopBot))
    TimerClose :: S => TopBot

-- The 'append' function
fun (++) :: [A],[A] -> [A] =
    a,[]      -> a
    [],a      -> a
    (b:bs),cs -> b : (bs ++ cs)

-- Boolean 'or'
fun (||) :: Bool,Bool -> Bool =
    False,False -> False
    _,_ -> True


-- Boolean 'and'
fun (&&) :: Bool,Bool -> Bool =
    True,True -> True
    _,_ -> False


fun isEmpty :: [A] -> Bool =
	[] -> True
	_ -> False


-- a // n
-- 12 // 4 = 3
-- 13 // 4 = 3
-- 13 - 4 = 9       (1)
-- 9 - 4 = 5        (2)
-- 5 - 4 = 1        (3)
-- 1 - 4 < 0
-- 4 - 4 = 0        (1)
-- 0 - 4 < 0

-- the division / does not work because it returns a double but like i didn't even know this language had doubles
-- so we are doing this shit now :,)
fun (//) :: Int, Int -> Int =  
    a, n -> switch 
        a - n < 0 -> 0
        True -> 1 + (a - n) // n

-- -- this function doesn't work because there aren't actually doubles in this language, so adding to a "double" doesn't even work somehow?
-- fun doubleToIntPositive :: Double -> Int =
--     x -> switch
--         x <= 0 -> 0
--         x > 0 -> 1 + doubleToIntPositive(x - 1.0)

-- -- also we can't even use decimals with multiplication because it doesn't even parse?
-- fun doubleToInt :: Double -> Int =
--     x -> switch
--         x < 0 -> -1 * doubleToIntPositive(-1 * x)
--         x >= 0 -> doubleToIntPositive(x)

-- -- we could implement integer division if this language actually had doubles
-- fun (//) :: Int, Int -> Int =  
--     a, n -> doubleToInt(a / n)

-- a % n = a - n * floor(a/n)
fun (%) :: Int, Int -> Int =  
    a, n -> a - n * (a // n)


-- floor(log2 n)
-- is floor division by 2 until lg(1) = 0 enough to take floor of log?
-- i hope so. i guess we will find out
-- assumes x >= 1
fun lg :: Int -> Int =
    1 -> 0
    n -> 1 + lg(n//2)


-- assumes x >= 0
fun intToString :: Int -> [Char] =
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    x -> intToString(x // 10) ++ intToString(x % 10)