

-- Console channel type with Timer handle

-- get user-def services are depreceated error
-- coprotocol S => Console = 
--     ConsolePut :: S => Get( [Char] | S)
--     ConsoleGet :: S => Put( [Char] | S)
--     ConsoleClose :: S => TopBot     
--     Timer :: S => Get(Int|S (*) Put(()|TopBot))
--     TimerClose :: S => TopBot

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

-- and now get match failure error
-- mpl: type check / semantic error:
--  •  Match failure with types

--         Timer (|)

--     and

--         Console (|)

--     arising from command

--         hput Timer on console

--     at line 16 and column 13 and command

--         hput ConsolePut on console_new

--     at line 20 and column 13