

protocol Echo => S =
    EchoSend :: Put([Char] | Get([Char] | S)) => S
    EchoClose :: TopBot => S


proc client :: Int | => Echo =
    val | => ch -> 
        if val == 0 then 
            on ch do
                hput EchoClose
                halt
        else do
            on ch do 
                hput EchoSend 
                put "Hello Server!" 
                get echo1
            client( val - 1 | => ch )


proc server :: | Echo => =
    | ch => -> 
        hcase ch of
            EchoSend -> do
                on ch do
                    get msg  
                    put msg
                server( | ch => )   -- recursion
            EchoClose -> halt ch


proc run = 
    | => -> plug 
        client( 2 | => ch )
        server( | ch => )