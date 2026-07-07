-- want to rename Get to GetPut?
-- GetPut( [Char] | S) = Get( [Char] | S) 

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc proc2 :: | Up([Char] | Down([Char] | TopBot)) => =
    | ch => -> do
        put "Hello World!" on ch
        get msg on ch
        halt ch

proc helloworld :: | Console => Up([Char] | Down([Char] | TopBot)) =
    | console => ch -> do
        get msg on ch
        put "Thanks!" on ch
        close ch

        hput ConsolePut on console
        put msg on console

        hput ConsoleClose on console
        halt console

proc run =
    | console => -> plug 
        helloworld( | console => ch )
        proc2( | ch => )