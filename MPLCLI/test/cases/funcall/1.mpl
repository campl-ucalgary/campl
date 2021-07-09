{-
a
b
c
-}
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun fstArg :: A,B,C -> A =
    a,b,c -> a

fun sndArg :: A,B,C -> B =
    a,b,c -> b

fun thrdArg :: A,B,C -> C =
    a,b,c -> c


proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put fstArg("a","b","c") on _console
        hput ConsolePut on _console
        put sndArg("a","b","c") on _console
        hput ConsolePut on _console
        put thrdArg("a","b","c") on _console

        hput ConsoleClose on _console
        halt _console

