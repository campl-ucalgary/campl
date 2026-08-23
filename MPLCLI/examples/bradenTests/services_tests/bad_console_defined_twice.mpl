

-- trying to get the Overlapping declarations error
defn 
    coprotocol S => Console = 
        ConsolePut :: S => Get( [Char] | S)
        ConsolePut :: S => Get( [Char] | S) -- this does not give an overlapping declarations error?
        ConsoleGet :: S => Put( [Char] | S)
        IntConsolePut :: S => Get( Int | S)
        IntConsoleGet :: S => Put( Int | S)
        ConsoleClose :: S => TopBot  

    coprotocol S => Console =               -- neither does this
        ConsolePut :: S => Get( [Char] | S)
        ConsolePut :: S => Get( [Char] | S) 
        ConsoleGet :: S => Put( [Char] | S)
        IntConsolePut :: S => Get( Int | S)
        IntConsoleGet :: S => Put( Int | S)
        ConsoleClose :: S => TopBot  

where
    coprotocol S => Console = 
        ConsolePut :: S => Get( [Char] | S)
        -- ConsolePut :: S => Get( [Char] | S) -- BUT this will give an overlapping declarations error!!
        ConsoleGet :: S => Put( [Char] | S)
        IntConsolePut :: S => Get( Int | S)
        IntConsoleGet :: S => Put( Int | S)
        ConsoleClose :: S => TopBot 

    coprotocol S => Console =                  -- somehow this does not?
        ConsolePut :: S => Get( [Char] | S)
        ConsoleGet :: S => Put( [Char] | S)
        IntConsolePut :: S => Get( Int | S)
        IntConsoleGet :: S => Put( Int | S)
        ConsoleClose :: S => TopBot 
    
    coprotocol S => StringConsole =           -- neither does this
        ConsolePut :: S => Get( [Char] | S)
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot 


proc process =
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 

proc run :: | Console => =           -- whether the type sig is here or not doesn't seem to matter
-- proc run =                           -- it either works or doesn't the same
    -- | console => -> process( | console =>)
    | console => -> on console do
        hput ConsolePut
        put "hello console" 
        hput ConsoleClose
        halt 

-- works when they both had exactly the same handles defined
-- works when the first one doesn't have ConsolePut, but the second one does

-- doesn't work when the first one has ConsolePut and the second one doesn't

-- mpl: type check / semantic error:
--  •  Match failure with types

--         Console (|)

--     and

--         Console (|)

--     arising from command

--         hput ConsolePut on console

--     at line 22 and column 9 and command

--         hput ConsoleClose on console

--     at line 24 and column 9
--  •  Match failure with types

--         Console (|)

--     and

--         Console (|)

--     arising from command

--         hput ConsolePut on console

--     at line 31 and column 9 and command

--         hput ConsoleClose on console

--     at line 33 and column 9
