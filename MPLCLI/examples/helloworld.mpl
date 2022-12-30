coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleClose :: S => TopBot 


proc helloworld :: | Console => = 
    | console => -> do
        hput ConsolePut on console 
        put "Hello World" on console

        hput ConsoleClose on console
        halt console

proc run = 
    | console => -> helloworld( |console=>)