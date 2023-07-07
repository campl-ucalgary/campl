coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 


fun f :: Int,Int -> Int =
    n,m -> n == m

proc helloworld :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console
        put "Enter your name" on console
        
        on console do
            hput ConsoleGet
            get name
            
            hput ConsolePut
            put "Hello,"
            hput ConsolePut
            put name
        
        hput ConsolePut on console 
        put "Hello World" on console
        
        hput ConsoleClose on console
        halt console

proc run = 
    | console => -> helloworld( |console=>)