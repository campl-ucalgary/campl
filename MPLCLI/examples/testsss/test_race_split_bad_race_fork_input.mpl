

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
    | => -> plug
        => ch2 -> do
            split ch2 into ch2a, ch2b 
            plug
                ch => ch2a -> do
                    close ch
                    process(21 | => ch2a)
                => ch2b, ch -> do
                    close ch
                    process(22 | => ch2b)
        ch2 => -> do
            race
                ch2 -> do
                    fork ch2 as 
                        ch2a -> do
                            get val2 on ch2a
                            halt ch2a
                        ch2b -> do
                            get val2 on ch2b
                            halt ch2b

