{-
pomeranian
pug
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p1 :: | => Put( ([Char], [Char]) | TopBot) (+) Put( ([Char], [Char]) | TopBot) =
    | => ch -> do
        split ch into ch0,ch1
        put ("pomeranian", "pug") on ch0
        put ("maltese", "yorkie") on ch1
        close ch1
        halt ch0

defn
    proc p2 :: | Console, Put( ([Char], [Char]) | TopBot) (+) Put( ([Char], [Char]) | TopBot) => =
        | _console, ch  => -> do
            fork ch as
                ch0 -> do
                    get (a,b) on ch0

                    hput ConsolePut on _console
                    put a on _console

                    hput ConsolePut on _console
                    put b on _console

                    hput ConsoleClose on _console
                    close _console
                    halt ch0
                ch1 -> do
                    get (a,b) on ch1

                    halt ch1

proc run =
    | _console => ->  do
        plug
            p1( |              => ch )
            p2( | _console, ch =>    )

