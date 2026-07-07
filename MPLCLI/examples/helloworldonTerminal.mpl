protocol StringTerminal  => S = 
    StringTerminalPut :: Put([Char] | S) => S 
    StringTerminalGet :: Get([Char] | S) => S 
    StringTerminalClose :: TopBot => S 


proc helloworld :: | => StringTerminal = 
    | => terminal -> do
        hput StringTerminalPut on terminal 
        put "Hello World! press any key to exit..." on terminal

        hput StringTerminalGet on terminal
        get input on terminal

        hput StringTerminalClose on terminal
        halt terminal

proc run = 
    | => terminal -> helloworld( | => terminal)