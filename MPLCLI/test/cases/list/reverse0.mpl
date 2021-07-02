{-
gfedcba
-}

fun reverse :: [A] -> [A] =
    lst -> 
        let fun go =
                [], lst1 -> lst1
                h:hs, lst1 -> go(hs, h:lst1)
        in go(lst, [])

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put reverse("abcdefg") on _console
        hput ConsoleClose on _console
        halt _console
