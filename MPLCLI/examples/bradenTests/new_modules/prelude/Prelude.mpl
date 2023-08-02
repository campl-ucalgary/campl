
-- This opens a terminal for which one may
-- output strings and get strings on.
protocol StringTerminal => S =
    StringTerminalGet :: Get([Char]| S) => S
    StringTerminalPut :: Put([Char]| S) => S
    StringTerminalClose :: TopBot => S

-- This allows console input and output of strings, and the
-- opening of new string terminals.
coprotocol S => Console =
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot
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

