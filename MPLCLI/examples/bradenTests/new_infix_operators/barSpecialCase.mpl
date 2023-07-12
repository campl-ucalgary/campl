-- This program doesn't compile.
-- It shows off how a '|>=' is a valid infix operator, but '|=>' is treated as 2 tokens. 
-- If it works, expect a parse error on line 30

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 


proc goodbyeWorld =
    |=> -> halt a -- this should parse correctly.

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> on console do
        hput ConsolePut
        put "enter anything to finish"
        hput ConsoleGet
        get _
        hput ConsoleClose
        halt

proc run = 
    | console => -> helloworld( |console=>)

fun (|>=) = -- this should work too.
    a,b -> a + b

fun (|=>) = -- this should give a parse error.
    a,b -> a + b