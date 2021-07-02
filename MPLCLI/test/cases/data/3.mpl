{-
c
-}
data MyTuple(A,B) -> S =
    MyTuple :: A,B -> S

fun getter :: MyTuple(MyTuple(A,B),MyTuple(C,D)) -> C =
    n -> case n of
        MyTuple(_,MyTuple(c,_)) -> c
    -- MyTuple(_,MyTuple(c,_)) -> c

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put getter(MyTuple(MyTuple("a", "b"),MyTuple("c", "d"))) on _console
        hput ConsoleClose on _console
        halt _console

