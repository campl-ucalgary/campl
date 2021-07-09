{-
pomeranian
morkie
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 



proc p0 :: [Char] | Get([Char] | TopBot) =>  =
    val |  ch  => -> do
        put val on ch
        halt ch

proc p1 :: | Console  =>  Get([Char] | TopBot) , Get([Char] | TopBot) =
    | _console => ch0,ch1  -> do
        get v0 on ch0
        get v1 on ch1

        hput ConsolePut on _console
        put v0 on _console

        hput ConsolePut on _console
        put v1 on _console

        hput ConsoleClose on _console
        close _console

        close ch0
        halt ch1


proc run =
    | _console => ->  do
        plug
            p0     ("pomeranian"|  ch0      =>     )
            p0     ("morkie"    |  ch1      =>     )
            p1     (            | _console  => ch0,ch1 )
