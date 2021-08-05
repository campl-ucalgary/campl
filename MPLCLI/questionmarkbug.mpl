coprotocol S => Console =
    ConsolePut   :: S => Get( [Char] | S) 
    ConsoleGet   :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc myputter :: |  => Put([Char] | TopBot)  =
    | => ch -> do
        put "banana" on ch
        halt ch

proc inpnegater :: | Neg(A),A => =
    | ch0,ch1  => -> 
        ch0 |=| neg ch1

proc outnegater :: | => Neg(A),A =
    | => ch0,ch1 -> 
        ch0 |=| neg ch1

proc mygetter :: | Console, Put([Char] | TopBot) =>  =
    | _console, ch  => -> do
        get n on ch
        close ch

        hput ConsolePut on _console
        put n on _console

        hput ConsoleClose on _console
        halt _console


proc run =
    | _console => -> do
        plug
            myputter(   |              => ch0    )
            inpnegater( | ch1,ch0      =>        )
            outnegater( |              => ch2,ch1)
            -- inpnegater( | ch2,ch3      =>        )
            mygetter(   | _console,ch2 => )
    

{-
proc run =
    | => _strterm0, _strterm1, _strterm2, _strterm3 -> do
-}

