

-- more than one console (with types)


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc run :: | Console, Console => =
    | console1, console2 => -> plug
        console1 => dummy -> do            
            close dummy
            on console1 do
                hput ConsolePut
                put "hello console 1"
                hput ConsoleClose
                halt
        console2, dummy => -> do            
            close dummy
            on console2 do
                hput ConsolePut
                put "hello console 2"
                hput ConsoleClose
                halt