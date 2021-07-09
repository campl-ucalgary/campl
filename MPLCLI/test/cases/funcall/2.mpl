{-
a
a
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 


fun test :: Bool -> [Char] =
    True -> "a"
    True -> "b"
    False -> "a"
    False -> "b"
    _ -> "c"

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put test(True) on _console
        hput ConsolePut on _console
        put test(False) on _console

        hput ConsoleClose on _console
        halt _console

