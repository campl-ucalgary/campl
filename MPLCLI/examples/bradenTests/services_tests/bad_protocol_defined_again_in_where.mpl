

-- what happens if we have a locally defined thing that overlaps?
    -- TODO: need to check that locally defined types are not used in
    -- definitions in a way that exposes them to the greater scope
    -- e.g. Weird should not be able to set itself to a locally defined type
    -- this should throw an error saying that it can't use this locally defined
    -- type in this way
-- the locally defined thing should shadow the globally defined one
-- but we should not be able to define a globally available type that
-- exposes a locally defined type

-- coprotocol S => Console =
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot  

coprotocol S => SendMsgs =
    SendMsg :: S => Get( [Char] | S)
    Close :: S => TopBot 

defn
    coprotocol S => Weird = 
        -- this should throw an error that says locally defined types can't be exposed like this
        Weird :: S => SendMsgs 

where
    coprotocol S => SendMsgs = 
        SendMsg :: S => Get( [Char] | S)
        Close :: S => TopBot 

proc process1 =
    | ch => -> on ch do
        hput Weird
        hput SendMsg     -- type check error here because this isn't the local SendMsgs (obviously)
        put "hello console"
        hput Close
        halt 

proc process2 =
    | console => ch -> do
        hcase ch of
            SendMsg -> do
                get msg on ch
                on console do
                    hput ConsolePut
                    put msg
                process2( | console => ch)
            Close -> do
                close ch
                on console do
                    hput ConsoleClose
                    halt

proc run :: | Console => =  
    | console => -> plug    
        process1( | ch => )
        console => ch -> do
            hcase ch of
                Weird -> process2( | console => ch)


-- we get the following error

-- mpl: type check / semantic error:
--  •  Match failure with types

--         SendMsgs

--     and

--         SendMsgs (|)

--     arising from command

--         hput Weird on ch

--     at line 34 and column 9 and command

--         hput SendMsg on ch

--     at line 35 and column 9
--  •  Cannot call `process1' at line 57 and column 9 (most likely because the term is invalid).