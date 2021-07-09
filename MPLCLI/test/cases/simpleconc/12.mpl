{-
pomeranian
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p0 :: | Get([Char] | TopBot) =>  =
    | ch0  => -> do
        put "pomeranian" on ch0
        halt ch0

proc p1 :: | => A, Neg(A) =
    | => ch0, ch1 -> do
        ch0 |=| neg ch1

proc p2 :: | A, Neg(A) => =
    | ch1, ch2 =>  -> do
        ch1 |=| neg ch2

proc p3 :: | Console => Get([Char] | TopBot) =
    | _console => ch  -> do
        get n on ch 
        close ch

        hput ConsolePut on _console
        put n on _console

        hput ConsoleClose on _console
        halt _console

proc run =
    | _console => -> do
        plug 
            p0( | ch0      =>          )
            p1( |          => ch0, ch1 )
            p2( | ch1, ch2 =>          )
            p3( | _console => ch2      )
