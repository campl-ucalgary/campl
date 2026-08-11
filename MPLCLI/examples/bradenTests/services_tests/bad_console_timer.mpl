

-- Console channel type with Timer handle


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot     
    Timer :: S => Get(Int|S (*) Put(()|TopBot))
    TimerClose :: S => TopBot

proc run :: | Console => =
    | console => -> do
        on console do
            hput Timer
            put 30000
        split console into console_new, timer
        on console_new do
            hput ConsolePut
            put "timer started for 30000 micro seconds?"
        on timer do
            get _
            close 
        on console_new do
            hput ConsolePut
            put "timer ended"
            hput ConsoleClose
            halt