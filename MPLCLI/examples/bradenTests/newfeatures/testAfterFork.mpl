

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 



proc oof =
    | => abc -> do
        on abc do
            put 0
            get _
            fork as
                b -> halt b
                c -> halt c
            put 0
            halt









proc run = 
    | console => -> do
        
        hput ConsoleClose on console
        halt console