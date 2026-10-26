

coprotocol S => Console =
    IntConsolePut :: S => Get(Int| S)
    IntConsoleGet :: S => Put(Int| S)
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc process = 
    x | => ch -> on ch do
        put (x + x + x - x - x)
        halt
    
proc run =
    | console => -> plug
        => ch2 -> do
            fork ch2 as 
                ch2a -> process(21 | => ch2a)
                ch2b -> process(22 | => ch2b)
        ch2, console => -> do
            race
                ch2 -> do
                    get val2 on ch2
                    close ch2
                    hput IntConsolePut on console
                    put val2 on console
                    hput ConsoleClose on console
                    halt console