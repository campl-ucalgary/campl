

-- this should give an out of scope error

proc run :: | Console, Timer => =
    | console, timer => -> do
        on timer do
            hput Timerz     -- right here right
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