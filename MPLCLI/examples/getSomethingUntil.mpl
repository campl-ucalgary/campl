coprotocol S => Console = 
    ConsoleGet :: S => Put([Char] | S) 
    ConsolePut :: S => Get([Char] | S) 
    ConsoleClose :: S => TopBot 

fun isExit :: [Char] -> Bool = 
    [] -> False
    ch:chs -> if ch == 'q' then True else False

proc getSomethingUtil :: | Console => = 
    | console => -> do
        hput ConsolePut on console 
        put "Enter something... (Enter q to exit)" on console

        hput ConsoleGet on console
        get inp on console

        if isExit(inp)
            then do
                hput ConsolePut on console 
                put "Bye..." on console
                hput ConsoleClose on console
                halt console
            else do
                hput ConsolePut on console
                put "you entered" on console
                hput ConsolePut on console
                put inp on console
                getSomethingUtil( |console=>)




proc run = 
    | console => -> getSomethingUtil( |console=>)