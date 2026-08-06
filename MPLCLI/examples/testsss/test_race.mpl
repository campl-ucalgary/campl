

coprotocol S => Console =
    IntConsolePut :: S => Get(Int| S)
    IntConsoleGet :: S => Put(Int| S)
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc process = 
    x | => ch -> on ch do
        put x
        halt
    
proc run =
    | console => -> plug
        process(2 | => ch1)
        process(1 | => ch2)
        ch1, ch2, console => -> do
            race
                ch1 -> do
                    get val on ch1
                    close ch1
                    hput IntConsolePut on console
                    put val on console
                    get val2 on ch2 
                    close ch2
                    hput IntConsolePut on console
                    put val2 on console
                    hput ConsoleClose on console
                    halt console
                ch2 -> do
                    get val on ch2
                    close ch2
                    hput IntConsolePut on console
                    put val on console
                    get val2 on ch1
                    close ch1
                    hput IntConsolePut on console
                    put val2 on console
                    hput ConsoleClose on console
                    halt console


