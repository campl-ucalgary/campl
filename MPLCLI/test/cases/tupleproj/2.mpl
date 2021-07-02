{-
b
-}

fun fst =
    (a,b) -> a

fun snd =
    (a,b) -> b

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put snd( ("a", "b") ) on _console
        hput ConsoleClose on _console
        halt _console

