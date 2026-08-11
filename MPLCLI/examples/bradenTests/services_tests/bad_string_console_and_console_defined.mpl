

-- StringConsole and Console definitions


coprotocol S => StringConsole = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    IntConsolePut :: S => Get( Int | S)
    IntConsoleGet :: S => Put( Int | S)
    ConsoleClose :: S => TopBot  


proc process =
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 


proc run :: | StringConsole => =
    -- | console => -> process( | console =>)
    | => console -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 

-- when the code was in the main process

-- mpl: type check / semantic error:
--  •  Illegal `hput' command at line 30 and column 9. Expected output polarity channel

--         console

--     at line 30 and column 9 to have a `protocol' but got a `coprotocol' instead.
--  •  Illegal `hput' command at line 32 and column 9. Expected output polarity channel

--         console

--     at line 32 and column 9 to have a `protocol' but got a `coprotocol' instead.

-- or when the code was moved up
-- mpl: type check / semantic error:
--  •  Could not match user provided type with inferred type. The given type was

--         forall . | StringConsole =>

--     and this could not be matched with the inferred type

--         forall . | Console (|) =>

--     arising from process

--         run

--     at line 27 and column 6 and process phrase

--         | console => -> process (| console =>)
