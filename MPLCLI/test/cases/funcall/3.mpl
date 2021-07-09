{-
a
c
e
a
c
e
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

data Test -> S =
    Test1 :: -> S
    Test2 :: -> S
    Test3 :: -> S


fun test0 :: Test -> [Char] =
    a -> case a of
        Test1 -> "a"
        Test1 -> "b"
        Test2 -> "c"
        Test2 -> "d"
        Test3 -> "e"
        Test3 -> "f"

fun test1 :: Test -> [Char] =
    a -> case a of
        Test2 -> "c"
        Test2 -> "d"
        Test3 -> "e"
        Test3 -> "f"
        Test1 -> "a"
        Test1 -> "b"


proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put test0(Test1) on _console
        hput ConsolePut on _console
        put test0(Test2) on _console
        hput ConsolePut on _console
        put test0(Test3) on _console

        hput ConsolePut on _console
        put test1(Test1) on _console
        hput ConsolePut on _console
        put test1(Test2) on _console
        hput ConsolePut on _console
        put test1(Test3) on _console


        hput ConsoleClose on _console
        halt _console

