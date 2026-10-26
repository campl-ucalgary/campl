

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
            race
                ch2 -> process(2 | => ch2)
        ch2 => -> do
            get x on ch2
            halt ch2

