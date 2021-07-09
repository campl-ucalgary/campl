{-
probably a yorkshire terrier
-}
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p1 :: | => Put(([Char], [Char]) | TopBot) =
    | => ch -> do
        put ("yorkshire", "dog") on ch
        halt ch

defn
    proc p2 :: | Console, Put( ([Char], [Char]) | TopBot) => =
        | _console, ch  => -> do
            get (a,b) on ch
            close ch

            case a of
                "yorkshire" -> do
                    hput ConsolePut on _console
                    put "probably a yorkshire terrier" on _console
                    haltConsole(| _console => )
                _ -> do
                    hput ConsolePut on _console
                    put "no idea!" on _console

                    haltConsole( | _console => )
where
    proc haltConsole =
        | _console =>  -> do
            hput ConsoleClose on _console
            halt _console


proc run =
    | _console => ->  do
        plug
            p1( |              => ch )
            p2( | _console, ch =>    )
