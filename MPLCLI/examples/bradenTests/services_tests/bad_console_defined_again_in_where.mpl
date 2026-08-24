

-- what happens if we have a locally defined thing that overlaps (currecntly isn't caught as an overlapping dec error)

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

coprotocol S => SendMsgs =
    SendMsg :: S => Get( [Char] | S)
    Close :: S => TopBot 

defn
    coprotocol S => Weird = 
        -- this is changing it to the local SendMsgs which then causes a type check error instead of a rename error
        Weird :: S => SendMsgs 

where
    -- this should probably cause an overlapping declaration error
    coprotocol S => SendMsgs = 
        IntPut :: S => Get( Int | S)
        IntGet :: S => Put( Int | S)
        Close :: S => TopBot 

proc process1 =
    | ch => -> on ch do
        hput Weird
        hput SendMsg            -- type check error here because this isn't the local SendMsgs (obviously)
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


-- instead of a rename error, we get the following error

-- mpl: type check / semantic error:
--  •  Match failure with types

--         SendMsgs

--     and

--         SendMsgs (|)

--     arising from command

--         hput Weird on ch

--     at line 28 and column 9 and command

--         hput SendMsg on ch

--     at line 29 and column 9
--  •  Cannot call `process1' at line 51 and column 9 (most likely because the term is invalid).