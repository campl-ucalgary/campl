

-- different name of "Console" channel type


coprotocol S => Snails = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot  

proc run :: | Snails => =
    | console => -> on console do
        hput ConsolePut
        put "hello Snails console"
        hput ConsoleClose
        halt