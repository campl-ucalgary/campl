protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol List(| A) => S =
    Cons :: A (*) S => S
    Nil :: A => S

proc cons :: | T, List( | T) => List( | T) =
    | t, ts => ts' -> do
        hput Cons on ts'
        fork ts' as
            t'' -> t |=| t''
            ts'' -> ts |=| ts''

proc nil :: | T => List(| T) =
    | t => ts -> do
        hput Nil on ts
        ts |=| t

proc foldList :: Store( | A, B => B) | B, List( | A) => B =
    p | x0, xs => y -> do
        hcase xs of
            Cons -> do
                split xs into head, tail
                plug
                    foldList(p | x0, tail => out)
                    use(p)( | head, out => y)
            Nil -> do
                use(p)( | xs, x0 => y)

proc append :: | List( | A), List( | A) => List( | A) =
    | x, y => xy -> foldList( store(cons) | x, y => xy)


proc reciever :: | List( | Put([Char] | TopBot)) => StringTerminal =
    | chList => strterm -> do
        hcase chList of
            Cons -> do
                split chList into head, tail
                get a on head
                hput StringTerminalPut on strterm
                put a on strterm
                close head
                reciever( | tail => strterm)
            
            Nil -> do
                get a on chList
                hput StringTerminalPut on strterm
                put a on strterm
                hput StringTerminalPut on strterm
                put "all data has recieved!" on strterm
                hput StringTerminalGet on strterm
                get _ on strterm
                hput StringTerminalClose on strterm
                close strterm
                halt chList


proc createList :: [Char], [Char] | => List( | Put([Char] | TopBot)) =
    a, b | => list -> do
        plug
            => ch0 -> do
                put a on ch0
                halt ch0
            nil( | ch0 => l1)
            => ch1 -> do
                put b on ch1
                halt ch1
            cons( | ch1, l1 => list)

proc run :: | => StringTerminal =
    | => strterm -> do    
        plug
            createList( "a", "b" | => l1)
            createList( "c", "d" | => l2)
            append( | l1, l2 => l3)
            reciever( | l3 => strterm)

