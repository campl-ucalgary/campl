protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we would have to fork for that... thats the idea
    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

proc run :: | Console => = 
    | console => -> do
        hput ConsoleStringTerminal on console
        split console into nconsole, negstrterm

        plug 
            negstrterm, strterm => -> negstrterm |=| neg strterm 

            nconsole => strterm -> do
                hput ConsoleClose on nconsole
                close nconsole

                hput StringTerminalGet on strterm
                get inp on strterm

                hput StringTerminalPut on strterm
                put inp on strterm

                hput StringTerminalClose on strterm
                halt strterm
