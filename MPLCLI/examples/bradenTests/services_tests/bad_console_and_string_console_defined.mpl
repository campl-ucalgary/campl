

-- Console and StringConsole definitions

-- now getting a rename error!!

-- mpl: rename error:
--  •  Overlapping declarations with `ConsoleClose' at line 11 and column 5 `ConsoleClose' at line 17 and column 5
--  •  Overlapping declarations with `ConsoleGet' at line 8 and column 5 `ConsoleGet' at line 16 and column 5
--  •  Overlapping declarations with `ConsolePut' at line 7 and column 5 `ConsolePut' at line 15 and column 5

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    IntConsolePut :: S => Get( Int | S)
    IntConsoleGet :: S => Put( Int | S)
    ConsoleClose :: S => TopBot  


coprotocol S => StringConsole = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc process =
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 


-- proc run :: | Console => =           -- doesn't work
-- proc run =                           -- works
proc run :: | StringConsole => =        -- works
    -- | console => -> process( | console =>)
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 

-- when the code was in the main process
-- or when i moved it up to the other process

-- if i made the type sig
-- proc run :: | StringConsole => =

-- IT WORKS??????

-- what happens if i remove type sig?
    -- it works lmao

-- what happens if i change it to use Console
    -- then it doesn't work LMAO

-- mpl: type check / semantic error:
--  •  Could not match user provided type with inferred type. The given type was

--         forall . | Console =>

--     and this could not be matched with the inferred type

--         forall . | StringConsole (|) =>

--     arising from process

--         run

--     at line 27 and column 6 and process phrase

--         | console => -> do
--         {
--           hput ConsolePut on console;
--           put "hello console" on console;
--           hput ConsoleClose on console;
--           halt console
--         }
