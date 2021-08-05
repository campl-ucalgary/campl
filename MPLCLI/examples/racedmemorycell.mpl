{- | Code for racing 2 memory cells which aquire the resource, then release it and any arbitrary time..
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S


protocol MemCell(A | ) => S =
    MemPut :: Put(A|S) => S 
    MemGet :: Get(A|S) => S 
    MemCls :: TopBot => S 

-- | a process for storing a sequential value in its argument,
-- which offers a protocol of 'MemCell' to get,put, and close
-- the memory cell
proc memCell :: A | MemCell(A | ) =>  = 
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> halt ch

-- | typical passer type to pass a channel between
-- 2 processes.
protocol Passer( A | ) => U =
    Passer :: MemCell(A | ) (+) (Neg(MemCell(A | )) (*) Put( () | U)) => U

-- | a leaf in the multi memory cell
proc leaf :: | => Put( () | Passer([Char] | )), StringTerminal =
    | => ch, _strterm -> do
        -- first, we ask the user to acquire the memory cell
        hput StringTerminalPut on _strterm
        put "Type anything to request the memory cell:" on _strterm

        hput StringTerminalGet on _strterm
        get userinp on _strterm

        -- this will acquire the memory cell by winning the race
        put () on ch

        -- usual memory cell stuff to get the passer.
        hput Passer on ch
        split ch into mem, negmemandnch

        -- Typical io
        ---------------
        hput StringTerminalPut on _strterm
        put "Current memory cell value:" on _strterm

        hput MemGet on mem
        get inp on mem

        hput StringTerminalPut on _strterm
        put inp on _strterm

        hput StringTerminalPut on _strterm
        put "Input a new memory cell value: " on _strterm

        hput StringTerminalGet on _strterm
        get userinp on _strterm

        hput MemPut on mem
        put userinp on mem
        -- end of typical io
        ---------------

        -- usual memory cell
        fork negmemandnch as 
            negmem -> negmem |=| neg mem 
            nch -> leaf(| => nch, _strterm)

-- | the root in the multi memory cell. This essentially acts
-- as the central mediator for the leaves. 
defn
    proc root :: | Put( () | Passer(A |)), Put( () | Passer(A | )) => MemCell( A | ) =
        | lch, rch => mem -> race
            lch -> go(| lch, rch => mem )
            rch -> go(| rch, lch => mem )

    proc go :: | Put( () | Passer(A |)), Put( () | Passer(A | )) => MemCell( A | ) =
        | winner, loser => mem -> do
            get _ on winner
            hcase winner of 
                Passer -> fork winner as 
                    rmem -> mem |=| rmem
                    negmemandnwinner -> do
                        split negmemandnwinner into negmem,nwinner
                        plug 
                            root( | nloser, nwinner => nmem )
                            negmem, nmem => -> negmem |=| neg nmem
                            loser => nloser -> do
                                get _ on loser
                                put () on nloser
                                nloser |=| loser


proc run =
    | => _strterm0, _strterm1 -> do
        plug
            memCell( "dogs!!" | mem => )
            ch => mem -> do
                split ch into lch, rch
                root( | lch,rch => mem)
            => ch, _strterm0, _strterm1 -> do
                fork ch as
                    lch -> leaf(| => lch, _strterm0)
                    rch -> leaf(| => rch, _strterm1)
