

coprotocol S => Console =
    IntConsolePut :: S => Get(Int| S)
    IntConsoleGet :: S => Put(Int| S)
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc process = 
    x | => ch -> on ch do
        put (x)
        halt
    
proc run =
    | console => -> plug
        process(1 | => ch1)
        => ch2 -> do
            fork ch2 as 
                ch2a -> process(21 | => ch2a)
                ch2b -> process(22 | => ch2b)
        ch1, ch2, console => -> do
            race
                ch1 -> do
                    get val on ch1
                    close ch1
                    hput IntConsolePut on console
                    put val on console
                    split ch2 into ch2a, ch2b 
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
                ch2 -> do
                    split ch2 into ch2a, ch2b
                    race
                        ch1 -> do
                            get val on ch1
                            close ch1
                            hput IntConsolePut on console
                            put val on console
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
                                    
                        ch2a -> do
                            get val on ch2a
                            close ch2a
                            hput IntConsolePut on console
                            put val on console
                            race
                                ch1 -> do
                                    get val2 on ch1
                                    close ch1
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
                                    get val3 on ch1
                                    close ch1
                                    hput IntConsolePut on console
                                    put val3 on console
                                    hput ConsoleClose on console
                                    halt console

                        ch2b -> do
                            get val on ch2b
                            close ch2b
                            hput IntConsolePut on console
                            put val on console
                            race
                                ch2a -> do
                                    get val2 on ch2a
                                    close ch2a
                                    hput IntConsolePut on console
                                    put val2 on console
                                    get val3 on ch1
                                    close ch1
                                    hput IntConsolePut on console
                                    put val3 on console
                                    hput ConsoleClose on console
                                    halt console
                                ch1 -> do
                                    get val2 on ch1
                                    close ch1
                                    hput IntConsolePut on console
                                    put val2 on console
                                    get val3 on ch2a
                                    close ch2a
                                    hput IntConsolePut on console
                                    put val3 on console
                                    hput ConsoleClose on console
                                    halt console
