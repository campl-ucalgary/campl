{-
banana
-}

-- trivial get/put example
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p1 :: | => Put([Char] | TopBot) =
    | => ch -> do
        put "banana" on ch
        halt ch

proc p2 :: | Console, Put([Char] | TopBot) => =
    | _console, ch  => -> do
        get inp on ch
        close ch

        hput ConsolePut on _console
        put inp on _console

        hput ConsoleClose on _console
        halt _console

proc run =
    | _console => ->  do
        plug
            p1( |              => ch )
            p2( | _console, ch =>    )
