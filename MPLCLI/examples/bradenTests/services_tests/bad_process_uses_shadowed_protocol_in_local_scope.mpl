
-- coprotocol S => Console =
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot  

coprotocol S => SendMsgs =
    SendMsg :: S => Get( [Char] | S)
    Close :: S => TopBot 

defn
    proc process1 =
        | ch => -> plug
            process1a( | => local_ch)
            process1b( | local_ch, ch => )

where
    -- this overlaps with the global coprotocol
    protocol SendMsgs => S = 
        SendMsg :: Put( [Char] | S) => S
        Close :: TopBot => S

    proc process1a =
        | => ch -> on ch do
            hput SendMsg 
            put "hello console"
            hput Close
            halt

    proc process1b = 
        | ch, output => -> do
            hcase ch of
                SendMsg -> do
                    get msg on ch
                    hput SendMsg on output
                    put msg on output
                    process1b( | ch, output => )
                Close -> do
                    close ch
                    on output do
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
        process2( | console => ch)

