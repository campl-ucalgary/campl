

-- Console channel hcase


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc run :: | Console => =
    | console => -> do
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

-- mpl: type check / semantic error:
--  •  Illegal `hcase' command. Expected input polarity channel from 

--         console

--     at line 22 and column 15 to have a `protocol' but got a `coprotocol' instead.
--  •  Expected polarity of Output but got the following channel of opposite polarity

--         console

--     at line 24 and column 28
--  •  Illegal `hcase' command. Expected input polarity channel from 

--         console

--     at line 22 and column 15 to have a `protocol' but got a `coprotocol' instead.
--  •  Expected polarity of Output but got the following channel of opposite polarity

--         console

--     at line 27 and column 40
--  •  Illegal `hcase' command. Expected input polarity channel from 

--         console

--     at line 22 and column 15 to have a `protocol' but got a `coprotocol' instead.
