

-- different name of "Console" channel type

-- mpl: assembler error:
-- No service type Snails for Input polarity channel console at line 12 and column 7

coprotocol S => Snails = 
    SnailsConsolePut :: S => Get( [Char] | S)
    SnailsConsoleGet :: S => Put( [Char] | S)
    SnailsConsoleClose :: S => TopBot  

proc run :: | Snails => =
    | console => -> on console do
        hput SnailsConsolePut
        put "hello Snails console"
        hput SnailsConsoleClose
        halt