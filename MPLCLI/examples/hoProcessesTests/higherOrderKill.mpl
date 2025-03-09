
protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S
        

proc p  =
    killer | => strterm -> do
        hput StringTerminalPut on strterm
        put "H" on strterm
        hput StringTerminalGet on strterm
        get _ on strterm
        use(killer)( | => strterm)
            

proc q =
    inp | => strterm -> p( store( | => strterm -> do
                                        hput StringTerminalPut on strterm
                                        put inp on strterm
                                        hput StringTerminalGet on strterm
                                        get _ on strterm
                                        hput StringTerminalClose on strterm
                                        halt strterm
                       ) | => strterm)

proc run :: | => StringTerminal =
    |  => strterm -> q( "K" | => strterm)
