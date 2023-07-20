coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 




fun f :: Int -> Int =
    a -> M.f(a)


proc run :: | Console => = 
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
        put "Done" on console
        
        hput ConsoleClose on console
        halt console