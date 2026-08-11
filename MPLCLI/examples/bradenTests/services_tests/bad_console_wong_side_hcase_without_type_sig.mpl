

-- Console channel wrong polarity, with hcase


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc process =
    | => console -> 
        hcase console of
            ConsolePut -> do
                get msg on console
                process( | => console)
                -- run( | => console)
            ConsoleGet -> do
                put "hello console" on console 
                process( | => console)
                -- run( | => console)
            ConsoleClose -> do
                halt console


proc run :: | => Console =
    | => console -> process( | => console)
    -- | => console -> do
    --     hcase console of
    --         ConsolePut -> do
    --             get msg on console
    --             run( | => console)
    --         ConsoleGet -> do
    --             put "hello console" on console 
    --             run( | => console)
    --         ConsoleClose -> do
    --             halt console

-- when the code was in the main process

-- IT WORKS??????
-- okay it doesn't actually *work* but the program runs
-- it just creates a terminal window
-- and then this terminal can't actually be interacted with because i guess
-- the abstract machine is just hanging trying to figure out wtf it's supposed to
-- do with an Hcase instruction??

-- and when i moved it up to the other process
-- it also "worked"

-- oh i found another weird error
-- if i try to call the run process from another process,
-- i get a rename error (with or without types)

-- mpl: rename error:
--  •  Out of scope identifier `run' at line 16 and column 17
--  •  Out of scope identifier `run' at line 19 and column 17

-- even when i uncomment the other process but leave the code in main
