

-- StringConsole and Console definitions

-- now getting rename error!!

-- mpl: rename error:
--  •  User-defined services have been depreceated. Remove overlapping declarations with services `ConsoleClose' at line 15 and column 5
--  •  User-defined services have been depreceated. Remove overlapping declarations with services `ConsoleGet' at line 14 and column 5
--  •  User-defined services have been depreceated. Remove overlapping declarations with services `ConsolePut' at line 13 and column 5

-- then if we comment this out
-- coprotocol S => StringConsole = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot  

-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     IntConsolePut :: S => Get( Int | S)
--     IntConsoleGet :: S => Put( Int | S)
--     ConsoleClose :: S => TopBot  


proc process =
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 


proc run :: | StringConsole => =
    -- | console => -> process( | console =>)
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 

-- when the code was in the main process

-- mpl: type check / semantic error:
--  •  Could not match user provided type with inferred type. The given type was

--         forall . | StringConsole =>

--     and this could not be matched with the inferred type

--         forall . | Console (|) =>

--     arising from process

--         run

--     at line 34 and column 6 and process phrase

--         | console => -> do
--         {
--           hput ConsolePut on console;
--           put "hello console" on console;
--           hput ConsoleClose on console;
--           halt console
--         }

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
