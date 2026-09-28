
-- coprotocol S => Console =
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot  

coprotocol S => SendMsgs =
    SendMsg :: S => Get( [Char] | S)
    Close :: S => TopBot 

defn 
    proc wrapper =
        | console => -> plug    
            process1( | ch => )
            process2( | console => ch)

where
    coprotocol S => SendMsgs = 
        SendMsg :: S => Get( [Char] | S)
        Close :: S => TopBot 
    
    proc process1 :: | SendMsgs => =
        | ch => -> on ch do
            hput SendMsg 
            put "hello console"
            hput Close
            halt
    proc process2 :: | Console => SendMsgs =
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
    | console => -> wrapper( | console => )

