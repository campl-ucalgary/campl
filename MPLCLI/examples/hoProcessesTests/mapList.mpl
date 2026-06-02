protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol List(| A) => S =
    Cons :: A (*) S => S
    Nil :: TopBot => S

data Maybe(A) -> S =
    Just :: A -> S
    Nothing :: -> S

proc cons :: | T, List( | T) => List( | T) =
    | t, ts => ts' -> do
        hput Cons on ts'
        fork ts' as
            t'' -> t |=| t''
            ts'' -> ts |=| ts''

proc nil :: | TopBot => List(| T) =
    | t => ts -> do
        hput Nil on ts
        ts |=| t


proc mapList :: Maybe(C), Store( Maybe(C) | A => B) | List( | A) => List( | B) =
    maybeData, p | la => lb ->
        hcase la of
            Cons -> do
                split la into hla, tla
                plug
                    use(p)(maybeData | hla => hlb)
                    mapList(maybeData, p | tla => tlb)
                    cons(| hlb, tlb => lb)

            Nil -> nil( | la => lb)


proc getToTopBot :: Maybe([Char]) | Get([Char] | TopBot) => TopBot =
    input | ch1 => ch2 ->
        case input of 
            Nothing -> do 
                        put "no data" on ch1
                        ch1 |=| ch2
            Just(str) -> do
                        put str on ch1
                        ch1 |=| ch2
                       
proc createList = 
    | a, b, c, d => ldcba ->
        plug
            nil( | a => la)
            cons( | b, la => lba)
            cons( | c, lba => lcba)
            cons( | d, lcba => ldcba)

proc killList :: | List( | TopBot) => StringTerminal =
    | list => strterm -> do
                hput StringTerminalPut on strterm
                put "Killer: I have a list of channels of type TopBot now!!, So I can kill them!!" on strterm

                hcase list of
                    Cons -> do
                        split list into head, tail
                        close head
                        hput StringTerminalPut on strterm
                        put "killed" on strterm
                        killList( | tail => strterm)

                    Nil -> do
                        hput StringTerminalPut on strterm
                        put "killed ! killing is done! press any key to exit!" on strterm
                        hput StringTerminalGet on strterm
                        get _ on strterm
                        hput StringTerminalClose on strterm
                        close list
                        halt strterm
                        
proc run :: | 
            TopBot, 
            Get([Char] | TopBot), 
            Get([Char] | TopBot), 
            Get([Char] | TopBot) => StringTerminal =
    | a, b, c, d => strterm -> do
        plug
            createList( | a, b, c, d => ldcba)
            mapList( Just("hii"), store(getToTopBot) | ldcba => idldcba)
            killList( | idldcba => strterm)

