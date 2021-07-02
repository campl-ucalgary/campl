{-
ab
-}
codata S -> App(A,B)  =
    App ::  A,S -> B

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put App('a' , (App := a -> a:"b")) on _console

        hput ConsoleClose on _console
        halt _console
