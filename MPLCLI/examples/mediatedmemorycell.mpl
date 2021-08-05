{- |  A modifcation of the memory cell example to use a third process
which acts as the central mediator for two procss which pass the memory cell
between each other.. 
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
protocol Passer( | M ) => U =
    Passer :: M (+) (Neg(M) (*) U) => U

-- | a leaf in the multi memory cell
proc leaf :: | => Passer(| MemCell([Char]|) ), StringTerminal =
    | => ch, _strterm -> do
        hput Passer on ch
        split ch into mem, negmemandnch

        -- useless stuff
        hput MemGet on mem
        get inp on mem

        hput StringTerminalPut on _strterm
        put inp on _strterm

        hput StringTerminalGet on _strterm
        get inp on _strterm

        hput MemPut on mem
        put inp on mem

        fork negmemandnch as 
            negmem -> negmem |=| neg mem 
            nch -> leaf(| => nch, _strterm)

-- | the root in the multi memory cell. This essentially acts
-- as the central mediator for the leaves. 
proc root :: | Passer(| MemCell( A | )), Passer(| MemCell( A | )) => MemCell( A | ) =
    | focused, offch => mem -> hcase focused of 
        Passer -> do
            fork focused as 
                rmem -> mem |=| rmem
                negmemandnfocused -> do
                    split negmemandnfocused into negmem,nfocused
                    plug 
                        root( | offch, nfocused => nmem )
                        negmem, nmem => -> negmem |=| neg nmem

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
