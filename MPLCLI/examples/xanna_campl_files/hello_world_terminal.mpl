protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

proc helloworld :: | => StringTerminal =
    | => terminal -> do
        hput StringTerminalPut on terminal
        put "Hello World!" on terminal

        hput StringTerminalGet on terminal
        get _ on terminal

        hput StringTerminalClose on terminal
        halt terminal

proc run =
    | => terminal -> helloworld( | => terminal )