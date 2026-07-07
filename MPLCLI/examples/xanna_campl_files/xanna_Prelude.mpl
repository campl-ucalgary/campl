

-- The 'append' function
fun (++) :: [A],[A] -> [A] =
    a,[]      -> a
    [],a      -> a
    (b:bs),cs -> b : (bs ++ cs)

fun append :: [A],[A] -> [A] =
    [],ts -> ts
    s:ss,ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
	[] -> []
	s:ss -> append(s, concat(ss))



protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

coprotocol S => Timer =
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

protocol MemCell (A | ) => M =
    MemPut :: Put(A|M) => M
    MemGet :: Get(A|M) => M 
    MemCls :: TopBot => M