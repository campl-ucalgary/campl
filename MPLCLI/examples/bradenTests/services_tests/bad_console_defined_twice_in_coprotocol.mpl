

-- trying to get the Overlapping declarations error

coprotocol 
    S => Console = 
        ConsolePut :: S => Get( [Char] | S)
        -- ConsolePut :: S => Get( [Char] | S) -- this now gives an overlapping declarations error!
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot  
    and
    S => Console =               -- this now also gives an overlapping dec error!
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

-- we get
-- mpl: rename error:
--  •  Overlapping declarations with `Console' at line 6 and column 10 `Console' at line 12 and column 10
--  •  Overlapping declarations with `ConsoleClose' at line 10 and column 9 `ConsoleClose' at line 15 and column 5
--  •  Overlapping declarations with `ConsoleGet' at line 9 and column 9 `ConsoleGet' at line 14 and column 5
--  •  Overlapping declarations with `ConsolePut' at line 7 and column 9 `ConsolePut' at line 13 and column 5