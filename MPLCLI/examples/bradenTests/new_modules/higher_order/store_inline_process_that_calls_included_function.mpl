include SampleModule (included_function| )

proc stored_process :: Int | => Put(Int | TopBot) =
    x | => ch -> on ch do
        put x
        halt

coprotocol S => Console =
    IntConsolePut :: S => Get(Int| S)
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc run =
    | console => -> plug 
        => ho_ch -> on ho_ch do
            put store( | => ch -> stored_process(included_function() | => ch))
            halt
        ho_ch => ch -> do
            on ho_ch do
                get stored_process
                close
            use(stored_process)(| => ch)
        ch, console => -> do
            get int on ch
            close ch
            on console do
                hput IntConsolePut
                put int
                hput ConsoleClose
                halt

-- this test passed

