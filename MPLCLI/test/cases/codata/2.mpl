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

fun fst :: MyTuple(A,B) -> A =
    (P0 := a) -> a

fun snd :: MyTuple(A,B) -> B =
    (P1 := b) -> b



proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put fst( ( P0 := -> "a", P1 := -> "b" ) ) on _console
        hput ConsolePut on _console
        put snd( ( P0 := -> "a", P1 := -> "b" ) ) on _console

        hput ConsoleClose on _console
        halt _console
