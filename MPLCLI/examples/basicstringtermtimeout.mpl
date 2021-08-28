protocol StringTerminal  => S = 
    StringTerminalPut :: Put([Char] | S) => S 
    StringTerminalGet :: Get([Char] | S) => S 
    StringTerminalClose :: TopBot => S 

coprotocol S => Timer =
    -- timer in microseconds
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

proc timedStringTerminal :: | Timer => StringTerminal  =
    | timer => strterm -> do  
        hput Timer on timer
        -- in seconds
        put (7 * 1000000) on timer
        split timer into ch0,ch1

        hput StringTerminalPut on strterm
        put "Please enter some input.." on strterm

        hput StringTerminalGet on strterm
        race 
            ch1 -> do
                get () on ch1

                get _ on strterm

                hput StringTerminalPut on strterm
                put "Timed out!" on strterm

                hput TimerClose on ch0
                hput StringTerminalClose on strterm

                close ch0
                close ch1
                halt strterm

            strterm -> do
                get inp on strterm

                hput StringTerminalPut on strterm
                put "Did not time out!" on strterm

                hput StringTerminalPut on strterm
                put inp on strterm

                plug
                    timer0, timer1 => nnch -> do
                        fork nnch as
                            nntimer0 with timer0 -> nntimer0 |=| timer0
                            nntimer1 with timer1 -> do
                                get _ on timer1
                                close timer1
                                halt nntimer1
                    nnch => nch -> do
                        split nnch into nntimer0,nntimer1
                        close nnch1
                        nntimer0 |=| nch
                
                    timedStringTerminal( | nch => strterm )

proc run =
    | timer => strterm -> do
        timedStringTerminal( |timer => strterm)

