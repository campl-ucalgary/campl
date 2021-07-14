{-
pomeranian
-}
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

codata S -> Fun(A,B) = 
    App :: A,S -> B

fun listMap :: Fun(A,B), [A] -> [B] = 
    _, [] -> []
    hfun, t:ts-> App(t,hfun) : listMap(hfun, ts)

proc run =
    | _console => -> do
        hput ConsolePut on _console
        put listMap(
            (App := c -> if c == 'b' then 'p' else c)
            ,  "bomeranian"
            ) 
            on _console

        hput ConsoleClose on _console
        halt _console
