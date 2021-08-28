{- | Code for racing 2 memory cells which aquire the resource, then release it and any arbitrary time..
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol  S => MemCell (A | ) =
    MemPut :: S => Get(A|S) 
    MemGet :: S => Put(A|S) 
    MemCls :: S => TopBot 

coprotocol S => Timer =
    -- timer in microseconds
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

-- | a process for storing a sequential value in its argument,
-- which offers a protocol of 'MemCell' to get,put, and close
-- the memory cell
proc memCell :: A |  => MemCell(A | ) =
    val | => ch  -> hcase ch of
        MemPut -> do
            get nval on ch
            memCell(nval | => ch )
        MemGet -> do
            put val on ch
            memCell(val | => ch )
        MemCls -> halt ch

-- | typical passer type to pass a channel between
-- 2 processes.
coprotocol S => Passer( | M ) =
    Passer :: S => M (*) (Neg(M) (+) Get( () | S)) 

-- | theh built in timer type 
coprotocol S => Timer =
    -- timer in microseconds
    Timer :: S => Get(Int | S (*) Put( () | TopBot))
    TimerClose :: S => TopBot

-- | a leaf in the multi memory cell
defn 
    proc leaf :: Int | Get( () | Passer( | Timer (*) MemCell([Char]| ))) => StringTerminal = 
        timeout | passer =>  strterm -> do
            -- first, we ask the user to acquire the memory cell
            hput StringTerminalPut on strterm
            put "Type anything to request the memory cell:" on strterm

            hput StringTerminalGet on strterm
            get userinp on strterm

            -- this will acquire the memory cell by winning the race
            put () on passer

            -- usual memory cell stuff to get the passer.
            hput Passer on passer
            split passer into m, negmandnpasser
            split m into timer, mem

            -- Typical io
            ---------------
            hput StringTerminalPut on strterm
            put "Current memory cell value:" on strterm

            hput MemGet on mem
            get inp on mem

            hput StringTerminalPut on strterm
            put inp on strterm

            hput StringTerminalPut on strterm
            put "Input a new memory cell value (be quick! You will time out): " on strterm

            -- prepare race for the string terminal
            hput StringTerminalGet on strterm

            -- set up the timer
            hput Timer on timer
            put timeout on timer 

            split timer into timer0, timer1


            race 
                strterm -> do
                    -- getting the user input
                    get userinp on strterm

                    -- working with the memory cell
                    hput MemPut on mem
                    put userinp on mem

                    plug 
                        timer0, timer1 => nntimer -> do
                            fork nntimer as
                                nntimer0 with timer0 -> nntimer0 |=| timer0
                                nntimer1 with timer1 -> do
                                    get _ on timer1
                                    close timer1
                                    halt nntimer1
                        nntimer => ntimer -> do
                            split nntimer into nntimer0,nntimer1
                            close nntimer1
                            nntimer0 |=| ntimer

                        ntimer,mem,negmandnpasser => strterm -> 
                            leafLoop(timeout | ntimer,mem,negmandnpasser => strterm) 

                timer1 -> do
                    get () on timer1

                    close timer1

                    -- we need to do this plug, so we can return the memory cell 
                    -- before we get user input 
                    plug 
                        z => strterm -> do
                            get _ on strterm
                            hput StringTerminalPut on strterm
                            put "Timed out!" on strterm
                            z |=| strterm
                        leafLoop(timeout | timer0,mem,negmandnpasser => z) 

    -- proc leafLoop :: Int | Get( () | Passer( | Timer (*) MemCell([Char]| ))) => StringTerminal = 
    proc leafLoop 
        :: Int 
        | Timer
        , MemCell([Char] |)
        , Neg (Timer  (*) MemCell([Char] |)) (+) Get (() | Passer(| Timer (*) MemCell([Char] |))) 
        => StringTerminal =
            timeout | ntimer,mem,negmandnpasser => strterm -> fork negmandnpasser as 
                negm -> plug
                    negm, z => -> negm |=| neg z 
                    ntimer,mem => z -> fork z as
                        l -> l |=| ntimer
                        r -> r |=| mem
                npasser -> leaf( timeout | npasser => strterm)
                

-- | the root in the multi memory cell. This essentially acts
-- as the central mediator for the leaves. 
defn
    proc root :: | M => Get( () | Passer(| M)), Get( () | Passer(| M)) =
        | m => lch, rch -> race
            lch -> go(| m => lch, rch )
            rch -> go(| m => rch, lch )

    proc go :: | M => Get( () | Passer(|M)), Get( () | Passer(|M)) =
        | m => winner, loser -> do
            get _ on winner
            hcase winner of 
                Passer -> fork winner as 
                    rm -> m |=| rm
                    negmandnwinner -> do
                        split negmandnwinner into negm,nwinner
                        plug 
                            root( | nm  => nloser, nwinner )
                            => negm, nm -> negm |=| neg nm 
                            nloser  => loser  -> do
                                get _ on loser
                                put () on nloser
                                nloser |=| loser

fun tIMEOUT :: -> Int  =
    -> 5 * 1000000


proc run :: | Timer => StringTerminal, StringTerminal =
    | timer => strterm0, strterm1 -> do
        plug
            memCell( "dogs!!" | => mem  )

            timer,mem => m -> fork m as
                l -> timer |=| l
                r -> mem |=| r

            m  => ch -> do
                split ch into lch, rch
                root( | m => lch,rch )
            ch => strterm0, strterm1 -> do
                fork ch as
                    lch -> leaf(tIMEOUT| lch => strterm0)
                    rch -> leaf(tIMEOUT| rch => strterm1)
