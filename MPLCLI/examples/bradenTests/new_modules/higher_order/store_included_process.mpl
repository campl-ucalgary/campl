include SampleModule ( |included_process)

coprotocol S => Console =    
    ConsolePut :: S => Get([Char]| S)
    ConsoleGet :: S => Put([Char]| S)
    ConsoleClose :: S => TopBot

proc run =
    | console => -> plug 
        => ho_ch -> on ho_ch do
            put store(included_process) -- this test was failing here
            halt
        ho_ch => ch -> do
            on ho_ch do
                get stored_process
                close
            use(stored_process)(| => ch)
        ch, console => -> do
            close ch
            on console do
                hput ConsolePut
                put "included process passed as higher-order message" 
                hput ConsoleClose
                halt



