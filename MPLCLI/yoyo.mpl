coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 


proc testing = 
    s | ch0  => ch1 -> case s of
        a:b -> do
            put a:s on ch1
            close ch1
            halt ch0
        [] -> do
            put s on ch1
            close ch1
            halt ch0



proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "sadf" on _console

        hput ConsoleClose on _console
        halt _console
