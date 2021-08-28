{-

    - Two guys are asking for a memory cell 

    - Whoever asks first gets the memory cell... Then, gets to determine if he can ge tthe seat that he wants.

    - If two people book the same seat, one after another, just one guy can sit there.


e.g. memory cell with just two seats.. Whoever books taht seat first gets the seat, or just one seat will do to start with 
    (for the memory cell)

Then, we can hopefully elaborate on that...

Then the question is three people...

Then the question is n people...

show the person the state, 
then release the state...

someone else can then do a booking on the state..

then, he actually does the booking..


2 stage booking:
    - looks at what's available (not locking the system up yet)

    - but when he actually makes the booking, he calls the memory cell, and tries to update, and may discover someone 
        has intervened...)

-- Further...
( think about this later )
- Timeouts... 

threadDelay 10000 


-- NIST (National institute of standards and technology)
-- (thinking about message passing / session types)
-- Demonstrate some programs (don't need to be super complicated!) !

I want 8!! I want 9!...
:q

-}

{-
protocol Passer( | M ) => S =
    Passer :: M (+) (Neg(M) (*) S)  => S
-}

-- | string terminal
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

-- | console
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

-- | list of concurrent types 
coprotocol S => ListP( | M)  =
    ConsP :: S => M (+) S
    NilP :: S => M

-- | raceable passer 
protocol Passer( | M ) => S =
    Passer :: M (+) ( Neg(M) (*) Put( () | S)) => S

-- | memory cell 
protocol MemCell (A | ) => S =
    MemPut :: Put(A|S) => S
    MemGet :: Get(A|S) => S
    MemCls :: TopBot => S

-- | memory cell process 
proc memCell :: A | MemCell(A | ) =>  = 
    val | ch => -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | ch => )
        MemGet -> do
            put val on ch
            memCell(val | ch => )
        MemCls -> halt ch

-- | a leaf in the multi memory cell
proc leaf :: | => Put( () | Passer( | MemCell([Char] | ))), StringTerminal =
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

-- | the node in the multi memory cell. This essentially acts
-- as the central mediator for the leaves. 
defn
    proc node :: | Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | ))) => MemCell( A | ) =
        | lch, rch => mem -> race
            lch -> nodeLoop(| lch, rch => mem )
            rch -> nodeLoop(| rch, lch => mem )

    proc nodeLoop :: | Put( () | Passer(| MemCell(A | ))), Put( () | Passer(| MemCell(A | ))) => MemCell( A | ) =
        | winner, loser => mem -> do
            get _ on winner
            hcase winner of 
                Passer -> fork winner as 
                    rmem -> mem |=| rmem
                    negmemandnwinner -> do
                        split negmemandnwinner into negmem,nwinner
                        plug 
                            node( | nloser, nwinner => nmem )
                            negmem, nmem => -> negmem |=| neg nmem
                            loser => nloser -> do
                                get _ on loser
                                put () on nloser
                                nloser |=| loser

{-
proc run =
    | => _strterm0, _strterm1 -> do
        plug
            memCell( "dogs!!" | mem => )
            ch => mem -> do
                split ch into lch, rch
                node( | lch,rch => mem)
            => ch, _strterm0, _strterm1 -> do
                fork ch as
                    lch -> leaf(| => lch, _strterm0)
                    rch -> leaf(| => rch, _strterm1)
-}

proc nilp :: | ListP(| M) => M =
    | up => m -> do
        hput NilP on up
        up |=| m

proc consp :: | ListP(| M) => M,ListP(| M) =
    | up => m, tail -> do
        hput ConsP on up
        fork up as 
            l -> l |=| m
            r -> r |=| tail

{-
proc run =
    | console => strterm -> do
        plug
            memCell( "dogs!!" | mem => )
            ch => mem -> do
                split ch into lch, rch
                node( | lch,rch => mem)
            => ch, _strterm0, _strterm1 -> do
                fork ch as
                    lch -> leaf(| => lch, _strterm0)
                    rch -> leaf(| => rch, _strterm1)
-}
