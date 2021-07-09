{-
pomeranian
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p0 :: |  => Put([Char] | Put([Char] | TopBot)) =
    | => ch0 -> do
        put "yorkshire terrier" on ch0
        put "pomeranian" on ch0
        halt ch0

proc p1 :: | Put([Char] | P ) => P =
    | ch0 => ch1 -> do
        get n on ch0
        ch0 |=| ch1

proc p2 :: | Console, Put([Char] | TopBot) => =
    | _console, ch1 =>  -> do
        get n on ch1
        close ch1

        hput ConsolePut on _console
        put n on _console

        hput ConsoleClose on _console
        halt _console

proc run =
    | _console => ->  do
        plug
            p0(|                => ch0 )
            p1(|           ch0  => ch1 )
            p2(| _console, ch1  =>     )
