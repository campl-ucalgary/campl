

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
        process(1 | => ch1)
        ch1, console => -> do
            race
                ch1 -> do
                    split ch1 into ch2a, ch2b 
                    race
                        ch2a -> do
                            get val2 on ch2a
                            close ch2a
                            hput IntConsolePut on console
                            put val2 on console
                            get val3 on ch2b
                            close ch2b
                            hput IntConsolePut on console
                            put val3 on console
                            hput ConsoleClose on console
                            halt console
                        ch2b -> do
                            get val2 on ch2b
                            close ch2b
                            hput IntConsolePut on console
                            put val2 on console
                            get val3 on ch2a
                            close ch2a
                            hput IntConsolePut on console
                            put val3 on console
                            hput ConsoleClose on console
                            halt console
                