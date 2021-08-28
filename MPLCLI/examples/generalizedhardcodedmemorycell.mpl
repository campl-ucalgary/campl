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
            lch -> rootLoop(| lch, rch => mem )
            rch -> rootLoop(| rch, lch => mem )

    proc rootLoop :: | Put( () | Passer(A |)), Put( () | Passer(A | )) => MemCell( A | ) =
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


-- | a node in the list
defn
    proc node :: |  Put( () | Passer(A |)), Put( () | Passer(A | )) => Put( () | Passer(A | )) = 
        | lch, rch =>  up -> race 
            lch -> nodeLoop(| lch, rch => up)
            rch -> nodeLoop(| rch, lch => up)

    proc nodeLoop :: |  Put( () | Passer(A |)), Put( () | Passer(A | )) => Put( () | Passer(A | )) = 
        | winner, loser => up -> do
            get _ on winner
            put () on up

            hput Passer on up 
            split up into upmem, negupmemandnup

            hcase winner of Passer -> fork winner as
                winnermem -> upmem |=| winnermem
                negmemandnwinner -> do
                    split negmemandnwinner into negmem,nwinner

                    fork negupmemandnup as
                        negupmem -> negupmem  |=| negmem
                        nup -> plug 
                            node( | nwinner, nloser => nup)
                            loser => nloser -> loser |=| nloser 

proc run =
    | => strterm0, strterm1, strterm2 -> do
        plug
            memCell( "dogs!!" | mem => )
            root( | ch2,up0 => mem)
            node(| ch0,ch1 => up0 )

            leaf(| => ch0, strterm0)
            leaf(| => ch1, strterm1)
            leaf(| => ch2, strterm2)
