protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

proc sender :: | => Put([Char] | TopBot) =
    | => b -> do
        put "x" on b
        halt b

proc reciever :: | Put([Char] | TopBot) => StringTerminal =
    | a => strterm -> do
        hput StringTerminalPut on strterm
        put "hii" on strterm
        get inp on a
        hput StringTerminalPut on strterm
        put inp on strterm
        hput StringTerminalGet on strterm
        get _ on strterm
        hput StringTerminalClose on strterm
        close strterm
        halt a


proc run :: | => StringTerminal =
    | => strterm ->    
        plug
            sender( | => b)
            reciever( | b => strterm)