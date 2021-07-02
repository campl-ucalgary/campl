{-
a
b
-}
codata S -> MyTuple(A,B)  =
    P0 ::  S -> A
    P1 ::  S -> B

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put P0( ( P0 := -> "a", P1 := -> "b" ) ) on _console
        hput ConsolePut on _console
        put P1( ( P0 := -> "a", P1 := -> "b" ) ) on _console

        hput ConsoleClose on _console
        halt _console
