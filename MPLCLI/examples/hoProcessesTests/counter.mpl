protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

proc p :: | A => A =
    | x => y -> x |=| y

proc q :: Int, Store( | A => A) | A => A, StringTerminal =
    n, p | x => y, strterm -> case n of
            0 -> do
                hput StringTerminalPut on strterm
                put "bye!" on strterm
                hput StringTerminalGet on strterm
                get _ on strterm
                hput StringTerminalClose on strterm
                close strterm
                x |=| y                   
            _ -> do
                hput StringTerminalPut on strterm
                put "hi!" on strterm
                plug
                    use(p)( | x => z)     
                    q( n - 1, p | z => y, strterm)

proc run :: | A => A, StringTerminal =
    | ch0 => ch1, strterm ->
            q(5, store(p) | ch0 => ch1, strterm)