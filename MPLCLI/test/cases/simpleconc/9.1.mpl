{-
pomeranian
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 


proc shifter :: | Get(A| TopBot) => Get(A| TopBot) =
    | ch0 => ch1 -> do
        get n on ch1
        close ch1

        put n on ch0
        halt ch0


proc p0 :: | Get([Char] | TopBot) =>  =
    |  ch  => -> do
        put "pomeranian" on ch
        halt ch

proc p1 :: | Console  =>  Get([Char] | TopBot) =
    | _console => ch  -> do
        get v on ch

        hput ConsolePut on _console
        put v on _console

        hput ConsoleClose on _console
        close _console

        halt ch 


proc run =
    | _console => ->  do
        plug
            p1     ( | _console  => ch4 )
            shifter( |  ch3      => ch2 )
            p0     ( |  ch1      =>     )
            shifter( |  ch2      => ch1 )
            shifter( |  ch4      => ch3 )
