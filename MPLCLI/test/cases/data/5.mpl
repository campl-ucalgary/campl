{-
abcd
-}

data MyList(A) -> S =
    MyCons :: A,S -> S
    MyNil ::      -> S

fun myListToList :: MyList(A) -> [A] =
    MyNil -> []
    MyCons(a,acc) -> a : myListToList(acc)

fun myListTest0 :: -> MyList(Char) =
    -> MyCons('a', MyCons('b', MyNil))

fun myListTest1 :: -> MyList(Char) =
    -> MyCons('c', MyCons('d', MyNil))

fun myInefficientListAppend :: MyList(A),  MyList(A) -> MyList(A) =
    MyNil, MyNil -> MyNil
    MyNil, MyCons(t, ts) -> MyCons(t, myInefficientListAppend(MyNil, ts ))
    MyCons(s, ss) , MyNil -> MyCons(s, myInefficientListAppend(ss, MyNil))
    MyCons(s, ss) , MyCons(t, ts) -> MyCons(s, myInefficientListAppend(ss, MyCons(t,ts )))


coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put myListToList(myInefficientListAppend(myListTest0, myListTest1)) on _console
        hput ConsoleClose on _console
        halt _console

