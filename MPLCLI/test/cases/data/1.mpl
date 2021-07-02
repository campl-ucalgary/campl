{-
a
-}

data MyTuple(A,B) -> S =
    MyTuple :: A,B -> S

fun fst :: MyTuple(A,B) -> A =
    MyTuple(a,b) -> a

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put fst(MyTuple("a","b")) on _console
        hput ConsoleClose on _console
        halt _console
