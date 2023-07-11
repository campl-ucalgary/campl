-- This program doesn't compile.
-- It shows off how the line numbers and token positions are correct even when converting to infix.


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

fun a =
    0 -> (+)("","") -- type mismatch with 'par' token
    n -> (*)("","") -- type mismatch with 'tensor' token

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