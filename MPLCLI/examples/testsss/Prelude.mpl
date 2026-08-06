
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


fun isEmpty :: [A] -> Bool =
	[] -> True
	_ -> False

-- a % n = a - n * floor(a/n)
fun (%) :: Int, Int -> Int =  
    a, n -> a - n * a / n


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