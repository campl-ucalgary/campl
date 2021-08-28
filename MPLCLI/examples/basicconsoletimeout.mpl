



coprotocol S => Console = 
    ConsoleGet :: S => Put([Char] | S) 
    ConsolePut :: S => Get([Char] | S) 
    ConsoleClose :: S => TopBot 

coprotocol S => Timer =
    -- timer in microseconds
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

proc timedConsole :: | Timer, Console  => =
    | timer, console => -> do  
        hput Timer on timer
        -- in seconds
        put (4 * 1000000) on timer
        split timer into ch0,ch1

        hput ConsolePut on console
        put "Please enter some input.." on console

        hput ConsoleGet on console
        race 
            ch1 -> do
                get () on ch1

                get _ on console

                hput ConsolePut on console
                put "Timed out!" on console

                hput TimerClose on ch0
                hput ConsoleClose on console

                close ch0
                close ch1
                halt console

            console -> do
                get inp on console

                hput ConsolePut on console
                put "Did not time out!" on console

                hput ConsolePut on console
                put inp on console

                plug
                    ch0, ch1 => nnch -> do
                        fork nnch as
                            nnch0 with ch0 -> nnch0 |=| ch0
                            nnch1 with ch1 -> do
                                get _ on ch1
                                close ch1
                                halt nnch1
                    nnch => nch -> do
                        split nnch into nnch0,nnch1
                        close nnch1
                        nnch0 |=| nch
                        
                    timedConsole( | nch, console => )


proc run =
    | timer,console =>  -> timedConsole( |timer, console=>)

