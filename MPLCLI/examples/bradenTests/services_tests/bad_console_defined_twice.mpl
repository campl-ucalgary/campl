

-- trying to get the Overlapping declarations error

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    -- ConsolePut :: S => Get( [Char] | S) -- this now gives an overlapping declarations error!
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

protocol NotAConsole => Z =            
    NotAConsoleClose :: TopBot => Z

coprotocol S => Console =               -- this now also gives an overlapping dec error!
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  


defn 
    coprotocol S => Console =                   -- and so do these!!
        ConsolePut :: S => Get( [Char] | S)
        -- ConsolePut :: S => Get( [Char] | S) -- this is now getting caught
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot  

    coprotocol S => Console =               -- so is this
        ConsolePut :: S => Get( [Char] | S)
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot  

where
    coprotocol S => Console =               -- this is not getting caught (even when we comment out the errors so we actually compile this part),
    -- because the check is only for the top level (i.e. all the global defs) and then within each where
    -- but it doesn't check the wheres against the global level
        ConsolePut :: S => Get( [Char] | S)
        -- ConsolePut :: S => Get( [Char] | S) -- BUT this will give an overlapping declarations error!!
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot 

    coprotocol S => Console =                  -- this does too with line 38!
        ConsolePut :: S => Get( [Char] | S)
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot 
    
    coprotocol S => StringConsole =
        ConsolePut :: S => Get( [Char] | S)     -- and these do too with line 39!
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot 

defn
    protocol Console => Z =             -- these are both giving overlapping dec errors
        ConsoleClose :: TopBot => Z

where
    coprotocol S => Console =               -- this is not getting caught either though...
        ConsolePut :: S => Get( [Char] | S)
        ConsoleGet :: S => Put( [Char] | S)
        ConsoleClose :: S => TopBot 

-- i think we still need the console in defn and where to have an error though...

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


-- very exciting! we now get 
-- mpl: rename error:
--  •  Overlapping declarations with `ConsolePut' at line 6 and column 9 `ConsolePut' at line 7 and column 9
--  •  Overlapping declarations with `ConsolePut' at line 14 and column 9 `ConsolePut' at line 15 and column 9
--  •  Overlapping declarations with `ConsolePut' at line 6 and column 9 `ConsolePut' at line 7 and column 9
--  •  Overlapping declarations with `ConsolePut' at line 14 and column 9 `ConsolePut' at line 15 and column 9

-- we are catching the error twice, so i might have gone a bit overkill, 
-- so lets fix it and see if we can get any more errors

-- mpl: rename error:
--  •  Overlapping declarations with `ConsolePut' at line 6 and column 5 `ConsolePut' at line 7 and column 5
--  •  Overlapping declarations with `ConsolePut' at line 23 and column 9 `ConsolePut' at line 24 and column 9

-- okay i made some more changes and now all the overlapping declarations in the global level are being caught
-- however, an overlapping dec of a locally defined Console with the global Console is not getting caught :,)
-- i have made another test file that shows why this is a problem.


-- previous notes
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
