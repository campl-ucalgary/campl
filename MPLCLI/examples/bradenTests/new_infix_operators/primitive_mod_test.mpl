-- testing new primitive integer div and mod operations

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    IntConsolePut :: S => Get( Int | S)
    IntConsoleGet :: S => Put( Int | S)
    ConsoleClose :: S => TopBot 

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> on console do
        hput ConsolePut
        put "testing 0 % 2"
        hput IntConsolePut
        put 0 % 2
        hput ConsolePut
        put "testing 4 % 2"
        hput IntConsolePut
        put 4 % 2
        hput ConsolePut
        put "testing 3 % 2"
        hput IntConsolePut
        put 3 % 2
        hput ConsolePut
        put "testing 12 % 3"
        hput IntConsolePut
        put 12 % 3
        hput ConsolePut
        put "testing 44 % 3"
        hput IntConsolePut
        put 44 % 3
        -- hput ConsolePut
        -- put "testing 4 % 0"
        -- hput IntConsolePut
        -- put 4 % 0               -- runtime error "mpl: divide by zero"
        -- hput ConsolePut
        -- put "testing 0 % 0"
        -- hput IntConsolePut
        -- put 0 % 0               -- runtime error "mpl: divide by zero"
        hput ConsolePut
        put "testing 4 / 3"
        hput IntConsolePut
        put 4 / 3
        -- hput ConsolePut
        -- put "testing 4 / 0"
        -- hput IntConsolePut
        -- put 4 / 0               -- runtime error "mpl: divide by zero"
        -- hput ConsolePut
        -- put "testing 0 / 0"
        -- hput IntConsolePut
        -- put 0 / 0               -- runtime error "mpl: divide by zero"
        hput ConsoleClose
        halt

proc run = 
    | console => -> helloworld( |console=>)