

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 



proc oof =
    | => abc -> do
        put 0 on abc
        get _ on abc
        fork abc as
            b -> halt b
            c -> halt c
        put 0 on abc
        halt abc









proc run = 
    | console => -> do
        
        hput ConsoleClose on console
        halt console