proc getFromTerminal :: 
    | Get( [Char] | T) => StringTerminal =
        parseA |ch => strterm -> do
            hput StringTerminalPut on strterm
            put "enter something" on strterm
            hput StringTerminalGet on strterm
            get input on strterm

            case App(input, parseA) of
                Just(a_val) -> do
                    put a_val on ch
                Nothing -> getFromTerminal(parseA|ch => strterm)

proc print :: 
  Parse A => | Get(A | TopBot) => StringTerminal =
    | ch => strterm -> do
       hput StringTerminalPut on strterm
       put "enter something" on strterm
       hput StringTerminalGet on strterm
       get input on strterm

       case parse(input) of
        Just(a_val) -> do
            put a_val on ch
        Nothing -> getFromTerminal(|ch => strterm)


proc run = 
    | => strterm -> do
