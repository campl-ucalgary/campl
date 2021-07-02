{-
ab
-}

data MyList(A) -> S =
    MyCons :: A,S -> S
    MyNil ::      -> S

fun myListToList :: MyList(A) -> [A] =
    MyNil -> []
    MyCons(a,acc) -> a : myListToList(acc)

fun myListTest :: -> MyList(Char) =
    -> MyCons('a', MyCons('b', MyNil))


coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put myListToList(myListTest) on _console
        hput ConsoleClose on _console
        halt _console
