

-- Console channel wrong polarity, with hcase


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

-- proc process =
--     | => channel -> on channel do
--         hput ConsolePut
--         put "hello console"
--         hput ConsoleClose
--         halt


proc run :: | => Console =
    -- | => console -> process( | => console)
    | => console -> do
        hcase console of
            ConsolePut -> do
                get msg on console
                run( | => console)
            ConsoleGet -> do
                put "hello console" on console 
                run( | => console)
            ConsoleClose -> do
                halt console

-- when the code was in the main process

-- IT WORKS??????

-- or when i moved it up to the other process
