

coprotocol S => Console =
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc process = 
    | => ch -> on ch do
        put "Hello World!"
        halt
    
proc run =
    | console => -> plug
        process( | => ch1)
        ch1, console => -> do
            race
                ch1 -> do
                    get val on ch1
                    close ch1
                    hput ConsolePut on console
                    put val on console
                    hput ConsoleClose on console
                    halt console


