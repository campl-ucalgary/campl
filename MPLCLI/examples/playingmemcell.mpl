protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) S)  => S

protocol MemCell (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

proc memCell :: A | MemCell(A | ) => =
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> do
            halt ch
        
proc p1 :: |  MemCell([Char]|) (+) (Neg(MemCell([Char]|)) (*) TopBot) => MemCell([Char]| ), StringTerminal =
    | passer => mem, _strterm -> do

        fork passer as 
            mmem with mem -> mmem |=| mem

            negmemandnpasser with _strterm -> do
                split negmemandnpasser into negmem, npasser
                plug 
                    npasser => z, _strterm -> do
                        hput MemGet on z
                        get inp on z

                        hput StringTerminalPut on _strterm
                        put "Receiving: " on _strterm

                        hput StringTerminalPut on _strterm
                        put inp on _strterm

                        hput StringTerminalPut on _strterm
                        put "Enter a string: " on _strterm

                        hput StringTerminalGet on _strterm
                        get ninp on _strterm

                        hput MemPut on z
                        put ninp on z

                        hput MemCls on z
                        close z

                        hput StringTerminalClose on _strterm
                        close _strterm

                        halt npasser
                        
                    z, negmem => -> negmem |=| neg z


proc p2 :: |  => MemCell([Char]|) (+) (Neg(MemCell([Char]|)) (*) TopBot) , StringTerminal =
    | => passer, _strterm -> do
        split passer into mem, negmemandnpasser

        hput MemGet on mem
        get inp on mem

        hput StringTerminalPut on _strterm
        put "Receiving: " on _strterm

        hput StringTerminalPut on _strterm
        put inp on _strterm

        hput StringTerminalPut on _strterm
        put "Enter a string: " on _strterm

        hput StringTerminalGet on _strterm
        get ninp on _strterm

        hput MemPut on mem
        put ninp on mem

        fork negmemandnpasser as 
            negmem with mem -> negmem |=| neg mem 
            npasser with _strterm -> do
                hput StringTerminalClose on _strterm
                close _strterm

                halt npasser
                    
proc run =
    | => _strterm0, _strterm1 -> do
        plug 
            p1( | passer => mem, _strterm0)
            p2( |        => passer, _strterm1)
            memCell( "I like dogs" | mem => )

