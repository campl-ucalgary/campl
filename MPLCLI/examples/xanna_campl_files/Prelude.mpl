
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

-- boolean and && and boolean or || added to compiler, removed from this version of the prelude

fun isEmpty :: [A] -> Bool =
	[] -> True
	_ -> False

-- type of division operator corrected so that compiler correctly performs integer division 

-- a % n = a - n * floor(a/n)
fun (%) :: Int, Int -> Int =  
    a, n -> a - n * (a / n)

-- floor(log2 n)
-- is floor division by 2 until lg(1) = 0 enough to take floor of log?
-- i hope so. i guess we will find out
-- assumes x >= 1
fun lg :: Int -> Int =
    1 -> 0
    n -> 1 + lg(n/2)

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
    x -> intToString(x / 10) ++ intToString(x % 10)