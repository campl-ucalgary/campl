

protocol Echo => S =
    EchoSend :: Put([Char] | Get([Char] | S)) => S
    EchoClose :: TopBot => S


 proc client :: | => Echo =
    | => ch -> 
        on ch do 
            hput EchoSend 
            put "Hello Server!" 
            get echo1

            hput EchoSend 			
            put "Goodbye Server!" 
            get echo2 
            
            hput EchoClose
            halt


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
        client( | => ch )
        server( | ch => )