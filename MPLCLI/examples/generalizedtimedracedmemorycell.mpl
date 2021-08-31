{- | Code for an aribtray number of memroy cells controlled by the console... 
Note that there is no intermediate list... 
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    ConsoleStringTerminal :: S => S (*) Neg(StringTerminal)

coprotocol  S => MemCell (A | ) =
    MemPut :: S => Get(A|S) 
    MemGet :: S => Put(A|S) 
    MemCls :: S => TopBot 

coprotocol S => ListP( | M)  =
    ConsP :: S => M (+) S
    NilP :: S => M

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
        MemCls -> do
            halt ch

-- | typical passer type to pass a channel between
-- 2 processes.
coprotocol S => Passer( | M ) =
    Passer :: S => M (*) (Neg(M) (+) Get( () | S)) 

defn 
    proc strTermLeaf :: Int | Get( () | Passer( | Timer (*) MemCell([Char]| ))) => StringTerminal = 
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
                            strTermLeafLoop(timeout | ntimer,mem,negmandnpasser => strterm) 

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
                        strTermLeafLoop(timeout | timer0,mem,negmandnpasser => z) 

    -- proc strTermLeafLoop :: Int | Get( () | Passer( | Timer (*) MemCell([Char]| ))) => StringTerminal = 
    proc strTermLeafLoop 
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
                npasser -> strTermLeaf( timeout | npasser => strterm)

-- | a node in the list
defn
    proc node :: | Get( () | Passer(| M)) =>  Get( () | Passer(| M)), Get( () | Passer(| M))  = 
        | up => lch, rch -> race 
            lch -> nodeLoop(|up => lch, rch)
            rch -> nodeLoop(|up => rch, lch)

    proc nodeLoop :: | Get( () | Passer(| M)) =>  Get( () | Passer(| M)), Get( () | Passer(| M))  = 
        | up => winner, loser -> do
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
                            node( | nup => nwinner, nloser )
                            nloser => loser -> loser |=| nloser 

-- | the root in the multi memory cell. This essentially acts
-- as the central mediator for the leaves. 
proc root :: | M => Get( () | Passer(| M))  =
    | m  => ch -> do
        get _ on ch
        hcase ch of Passer -> fork ch as 
            rm -> m |=| rm
            negmandnch -> do
                split negmandnch into negm,nch
                plug 
                    root( | nm => nch )
                    => negm, nm -> negm |=| neg nm


-- | The root of the multi memory cell which again acts as the
-- central. Moreover, this allows a console to help spawn additional 
-- clients... 
defn
    proc controlledRoot 
        ::  Int
        | Console, Timer (*) MemCell( [Char] | ) 
        => Get( () | Passer(| Timer (*) MemCell([Char]|))) = 
            timeout| console, m => racedpasser -> do
                hput ConsolePut on console
                put "Type anything and press enter to spawn a new client..." on console

                hput ConsoleGet on console
                controlledRootLoop(timeout | console, m => racedpasser)


    proc controlledRootLoop 
        :: Int
        | Put( [Char] | Console)
        , Timer (*) MemCell( [Char] |) 
        => Get( () | Passer(| Timer (*) MemCell([Char]|))) =
            timeout | console, m => racedpasser -> race 
                console -> do
                    get inp on console

                    hput ConsolePut on console
                    put "Spawning new client..." on console

                    hput ConsoleStringTerminal on console
                    split console into nconsole, negstrterm

                    plug 
                        negstrterm, strterm => -> negstrterm |=| neg strterm


                        node( | up => racedpasser, nleaf)
                        strTermLeaf( timeout | nleaf => strterm)

                        controlledRoot( timeout | nconsole, m => up)
                -- duplicated code from 'root' above.
                racedpasser -> do
                    get _ on racedpasser
                    hcase racedpasser of Passer -> fork racedpasser as 
                        rm -> m |=| rm
                        negmandnracedpasser -> do
                            split negmandnracedpasser into negm,nracedpasser
                            plug 
                                controlledRootLoop( timeout | console,nm => nracedpasser )
                                => negm, nm -> negm |=| neg nm
-- | time in seconds for the time out
fun tIMEOUT :: -> Int  =
    -> 5 * 1000000

proc run = 
    | timer, console => strterm -> plug
        memCell( "dogs!!" | => mem  )

        timer, mem => m -> fork m as
            l -> l |=| timer
            r -> r |=| mem
        controlledRoot( tIMEOUT | console, m => tail)  
        strTermLeaf( tIMEOUT  | tail => strterm)
