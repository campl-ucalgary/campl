

-- this should give an overlapping declarations error and not necessarily a 
-- services depreiated error
coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot     

coprotocol S => Timer = 
    Timer :: S => Get(Int|S (*) Put(()|TopBot))
    TimerClose :: S => TopBot

coprotocol S => Timer = 
    Timer :: S => Get(Int|S (*) Put(()|TopBot))
    TimerClose :: S => TopBot

proc run :: | Console, Timer => =
    | console, timer => -> do
        on timer do
            hput Timer
            put 30000
        split timer into timer_new, time_counter
        on console do
            hput ConsolePut
            put "timer started for 30000 micro seconds?"
        on timer_new do
            hput TimerClose
            close
        on time_counter do
            get _
            close 
        on console do
            hput ConsolePut
            put "timer ended"
            hput ConsoleClose
            halt