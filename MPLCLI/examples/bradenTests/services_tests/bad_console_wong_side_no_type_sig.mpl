

-- Console channel wrong polarity


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc process =
    | => channel -> on channel do
        hput ConsolePut
        put "hello Snails console"
        hput ConsoleClose
        halt


proc run =
    | => console -> process( | => console)

-- when the code was in the main process

-- mpl: type check / semantic error:
--  •  Illegal `hput' command at line 13 and column 9. Expected output polarity channel

--         console

--     at line 13 and column 9 to have a `protocol' but got a `coprotocol' instead.
--  •  Illegal `hput' command at line 15 and column 9. Expected output polarity channel

--         console

--     at line 15 and column 9 to have a `protocol' but got a `coprotocol' instead.


-- or when i moved it up to the other process

-- mpl: type check / semantic error:
--  •  Illegal `hput' command at line 13 and column 9. Expected output polarity channel

--         channel

--     at line 13 and column 9 to have a `protocol' but got a `coprotocol' instead.
--  •  Illegal `hput' command at line 15 and column 9. Expected output polarity channel

--         channel

--     at line 15 and column 9 to have a `protocol' but got a `coprotocol' instead.
--  •  Cannot call `process' at line 20 and column 21 (most likely because the term is invalid).