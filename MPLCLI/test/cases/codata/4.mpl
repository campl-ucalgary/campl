{-
abc
-}

codata S -> App(A,B,C)  =
    App ::  A,B,S -> C

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put App('a', 'b', (App := a,b -> a:b:"c")) on _console

        hput ConsoleClose on _console
        halt _console
