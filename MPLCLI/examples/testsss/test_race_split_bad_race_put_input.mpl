

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

proc process2_a =
    | winner, loser => output -> do
        get x on winner
        put x on output
        get y on loser
        put y on output
        close winner
        close loser
        halt output


proc process2 = 
    | chx, chy => chz -> do
        race
            chx -> process2_a( | chx, chy => chz)
            chy -> process2_a( | chy, chx => chz)

    
proc run =
    | console => -> plug
        => chx -> process("ch x" | => chx)
        => chy -> process("ch y" | => chy)
        chx, chy => chz -> process2( | chx, chy => chz)
        console, chz => -> do
            get msg1 on chz
            hput ConsolePut on console
            put msg1 on console
            get msg2 on chz
            hput ConsolePut on console
            put msg2 on console
            close chz
            hput ConsoleClose on console
            halt console



