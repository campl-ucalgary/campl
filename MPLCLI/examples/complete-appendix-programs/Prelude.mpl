-- Prelude from Appendix 1

-- combines code from Examples 14, 31, 32:
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

protocol IntTerminal => S =
    IntTerminalPut :: Put( Int | S) => S
    IntTerminalGet :: Get( Int | S) => S 
    IntTerminalClose :: TopBot => S

protocol CharTerminal => S =
    CharTerminalPut :: Put( Char | S) => S
    CharTerminalGet :: Get( Char | S) => S 
    CharTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 
    ConsoleStringTerminal :: S => S (+) Neg(StringTerminal)

coprotocol S => IntConsole =
    IntConsolePut :: S => Get( Int | S) 
    IntConsoleGet :: S => Put( Int | S) 
    IntConsoleClose :: S => TopBot 

coprotocol S => CharConsole =
    CharConsolePut :: S => Get( Char | S) 
    CharConsoleGet :: S => Put( Char | S) 
    CharConsoleClose :: S => TopBot 

coprotocol S => Timer =
    Timer :: S => Get(Int|S (*) Put(()|TopBot))
    TimerClose :: S => TopBot

-- from Example 19:
fun isEmpty :: [A] -> Bool =
    []  ->  True
    _   ->  False

-- from Example 20:
fun (++) :: [A],[A] -> [A] =
	a,[]			-> a
	[],a 			-> a
	(b:bs),cs	-> b : (bs ++ cs)  
