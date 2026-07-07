coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p :: [Char] | Console => =
    msg | console => -> do
        hput ConsolePut on console
        put msg on console
        hput ConsoleGet on console
        get inp on console
        hput ConsoleClose on console
        halt console

proc run =
    | console1, console2 => ->
        plug
            console1, z => -> do
                close z
                p("hello from p1" | console1 => )
            console2 => z -> do
                close z
                p("hello from p2" | console2 => )