protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

proc p :: | => StringTerminal =
    | => strterm -> do
        hput StringTerminalPut on strterm
        put "Hello From the stored proc!" on strterm
        hput StringTerminalGet on strterm
        get _ on strterm
        hput StringTerminalClose on strterm
        halt strterm

proc server :: | => Put( Store( | => StringTerminal) | TopBot) =
    | => ch -> do
        put store(p) on ch
        halt ch

proc client :: | Put( Store( | => StringTerminal) | TopBot) => StringTerminal =
    | ch => strterm -> do
        hput StringTerminalPut on strterm
        put "Hello from the client" on strterm
        get p on ch
        close ch
        use(p)( | => strterm)
        

proc run =
    | => strterm -> plug
        server( | => ch )
        client( | ch => strterm)