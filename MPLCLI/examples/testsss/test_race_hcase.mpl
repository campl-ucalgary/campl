

coprotocol S => Console =
    IntConsolePut :: S => Get(Int| S)
    IntConsoleGet :: S => Put(Int| S)
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

protocol SendMsgs => Z =
    Send :: Put([Char]|Z) => Z
    End :: TopBot => Z

proc process = 
    x | => ch -> on ch do
        hput Send
        put x
        hput End
        halt


proc printer =
    | console, ch => -> do
        hcase ch of
            Send -> do
                get msg on ch
                hput ConsolePut on console
                put msg on console
                printer( | console, ch => )
            End -> do
                close ch
                hput ConsoleClose on console
                halt console


proc racer =
    | console, ch1, ch2 => -> do
        hcase ch1 of
            Send -> do 
                get msg on ch1
                hput ConsolePut on console
                put msg on console
                race 
                    ch1 -> racer( | console, ch1, ch2 => )
                    ch2 -> racer( | console, ch2, ch1 => )
            End -> do 
                close ch1
                printer( | console, ch2 => )



proc run =
    | console => -> plug
        process("1" | => ch1)
        process("2" | => ch2)
        ch1, ch2, console => -> do
            race
                ch1 -> racer( | console, ch1, ch2 => )
                ch2 -> racer( | console, ch2, ch1 => )

