{- | A ticket booking system based on @./generalizedtimedracedmemorycell.mpl@. 
Pretty much the most straightforward application of that program
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

fun append :: [A],[A] -> [A] =
    [],ts -> ts
    s:ss,ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
	[] -> []
	s:ss -> append(s, concat(ss))
    
-- | ticket data type. 
codata S -> Ticket = 
    Name :: S -> [Char]
    Description :: S -> [Char]
    Number :: S -> Int

-- | wrapper for making a ticket
fun mkTicket :: [Char],[Char],Int -> Ticket = 
    name, description,number -> (Name := -> name,  Description := -> description, Number := -> number)


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

data Maybe(A) -> S =
    Just :: A -> S
    Nothing :: -> S

-- | parses an int 
defn 
    fun pInt :: [Char] -> Maybe(Int) =
        [] -> Nothing
        str -> go(Just(0), str)

where
    fun charToDigit :: Char -> Maybe(Int) =
        '0' -> Just(0)
        '1' -> Just(1)
        '2' -> Just(2)
        '3' -> Just(3)
        '4' -> Just(4)
        '5' -> Just(5)
        '6' -> Just(6)
        '7' -> Just(7)
        '8' -> Just(8)
        '9' -> Just(9)
        _   -> Nothing


    fun step :: Maybe(Int),Char -> Maybe(Int) =
        Nothing, _ -> Nothing
        Just(n), c -> case charToDigit(c) of
            Just(nn) -> Just(n * 10 + nn)
            Nothing -> Nothing

    fun go :: Maybe(Int),[Char] -> Maybe(Int) =
        Nothing, _ -> Nothing
        res, [] -> res
        res, s:ss -> go(step(res,s), ss)

-- | time in seconds for the time out of the clients
fun tIMEOUT :: -> Int  =
    -> 5 * 1000000

defn
    fun showInt :: Int -> [Char] =
        0 -> ['0']
        n -> go(n, [])
where
    -- this does integer division for a,b as input
    -- and outputs (q,r) where q is the integer division and r 
    -- is the remainder.
    fun divqr :: Int, Int -> (Int, Int) =
        a, b -> if a < b
            then (0, a)
            else case divqr(a - b, b) of
                (q, r) ->  (q + 1, r)

    fun intToDigit :: Int -> Char =
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        -- in the use cases of this program, this will never happen!
        _ -> '#'

    fun go :: Int, [Char] -> [Char] =
        0, acc -> acc
        n, acc -> case divqr(n, 10) of
            (q, r) -> go(q, intToDigit(r):acc)

fun showTickets :: [Ticket] -> [Char] = 
    [] -> ""
    (Name := name, Description := description, Number := number ):ss -> 
        concat(
            [ "Ticket name: "
            , name
            , "\n"
            , "Description: "
            , description
            , "\n"
            , "Ticket number: "
            , showInt(number)
            , "\n"
            , "\n"
            , showTickets(ss)
            ]
            )
        

-- | typical passer type to pass a channel between
-- 2 processes.
coprotocol S => Passer( | M ) =
    Passer :: S => M (*) (Neg(M) (+) Get( () | S)) 

defn 
    proc clientLeaf :: [Ticket] | Get( () | Passer( | Timer (*) MemCell([Ticket]| ))) => StringTerminal = 
        curtickets | passer =>  strterm -> do
            hput StringTerminalPut on strterm
            put "Type: <:my-tickets>, <:view>, or <:book>." on strterm

            hput StringTerminalGet on strterm
            get userinp on strterm

            case userinp of
                ":my-tickets" -> do
                    hput StringTerminalPut on strterm
                    put "The current tickets you hold are as follows." on strterm

                    hput StringTerminalPut on strterm
                    put showTickets(curtickets) on strterm

                    clientLeaf( curtickets | passer => strterm)
                    
                ":view" -> do
                    hput StringTerminalPut on strterm
                    put "Viewing available tickets...." on strterm

                    put () on passer

                    hput Passer on passer
                    split passer into m, negmandnpasser
                    split m into timer, mem

                    hput MemGet on mem
                    get inp on mem

                    hput StringTerminalPut on strterm
                    put showTickets(inp) on strterm

                    clientLeafLoop( curtickets | timer,mem, negmandnpasser => strterm )

                ":book" -> do
                    -- start of duplicated from ":view"
                    hput StringTerminalPut on strterm
                    put "Viewing available tickets..." on strterm

                    put () on passer
                    
                    hput Passer on passer
                    split passer into m, negmandnpasser
                    split m into timer, mem

                    hput MemGet on mem
                    get inp on mem

                    hput StringTerminalPut on strterm
                    put showTickets(inp) on strterm
                    -- end of duplicated from ":view"

                    hput StringTerminalPut on strterm
                    put "Please enter the ticket number you would like to book (you will timeout if you take too long!)..." on strterm

                    hput StringTerminalGet on strterm

                    -- set up the timer
                    hput Timer on timer
                    put tIMEOUT on timer 

                    split timer into timer0, timer1

                    race 
                        strterm -> do
                            -- getting the user input
                            get userinp on strterm

                            
                            case pInt(userinp) of 
                                Just(userinp) -> case focusTicket(userinp, inp) of
                                    Just( (tkt, tkts) ) -> do
                                        hput MemPut on mem
                                        put tkts on mem

                                        hput StringTerminalPut on strterm
                                        put "Booking succeeded!" on strterm

                                        bookLoop( tkt : curtickets | timer0, timer1, mem, negmandnpasser => strterm)
                                        
                                    Nothing -> do
                                        hput StringTerminalPut on strterm
                                        put "Booking failed! Ticket does not exist.. Please book again..." on strterm

                                        bookLoop( curtickets | timer0, timer1, mem, negmandnpasser => strterm)
                                Nothing ->  do
                                    hput StringTerminalPut on strterm
                                    put "Booking failed! You did not input a ticket number.. Please book again..." on strterm

                                    bookLoop( curtickets | timer0, timer1, mem, negmandnpasser => strterm)


                        timer1 -> do
                            get () on timer1

                            close timer1

                            -- we need to do this plug, so we can return the memory cell 
                            -- before we get user input 
                            plug 
                                z => strterm -> do
                                    get _ on strterm
                                    hput StringTerminalPut on strterm
                                    put "Booking failed! Sorry you timed out! Please try and book again..." on strterm

                                    z |=| strterm
                                clientLeafLoop(curtickets | timer0,mem,negmandnpasser => z) 


                    
                    
                _ -> do
                    hput StringTerminalPut on strterm
                    put "Sorry, I didn't understand your input." on strterm
                    clientLeaf( curtickets | passer => strterm)

    proc clientLeafLoop 
        :: [Ticket] 
        | Timer
        , MemCell([Ticket] |)
        , Neg (Timer  (*) MemCell([Ticket] |)) (+) Get (() | Passer(| Timer (*) MemCell([Ticket] |))) 
        => StringTerminal =
            curtickets | ntimer,mem,negmandnpasser => strterm -> fork negmandnpasser as 
                negm -> plug
                    negm, z => -> negm |=| neg z 
                    ntimer,mem => z -> fork z as
                        l -> l |=| ntimer
                        r -> r |=| mem
                npasser -> clientLeaf( curtickets | npasser => strterm)


    proc bookLoop 
        :: [Ticket] 
        | Timer
            -- ^ the timer
        , Put( () | TopBot) 
            -- ^ the timer which has been peeled back
        , MemCell([Ticket] |)
        , Neg (Timer  (*) MemCell([Ticket] |)) (+) Get (() | Passer(| Timer (*) MemCell([Ticket] |))) 
        => StringTerminal = 
            curtickets | timer0,timer1,mem,negmandnpasser => strterm -> plug 
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
                    clientLeafLoop(curtickets | ntimer,mem,negmandnpasser => strterm) 

where
    fun focusTicket :: Int, [Ticket] -> Maybe( (Ticket, [Ticket]) )= 
        n, tickets -> 
            let fun go =
                    [] -> Nothing
                    s:ss -> if Number(s) == n
                        then Just( (s, ss) )
                        else case go(ss) of 
                            Nothing -> Nothing
                            Just( (t,ts) ) -> Just( (t,s:ts) )
            in go(tickets)


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
    proc server 
        :: 
        Int
        | Console, Timer (*) MemCell( [Ticket] | ) 
        => Get( () | Passer(| Timer (*) MemCell([Ticket]|))) = 
            freshtknum | console, m => racedpasser -> do
                hput ConsolePut on console
                put "Type <:new-client> <:new-ticket <NAME> <DESCRIPTION>> <:view> " on console

                hput ConsoleGet on console

                serverLoop( freshtknum | console, m => racedpasser)

    proc serverLoop 
        :: 
        Int
        | Put( [Char] | Console)
        , Timer (*) MemCell( [Ticket] |) 
        => Get( () | Passer(| Timer (*) MemCell([Ticket]|))) =
            -- we need to race the console and the memory cell stuff
            -- since these inputs should not depend on each other.
            freshtknum | console, m => racedpasser -> race 
                console -> do
                    get inp on console

                    case pArgs(inp) of 
                        [":new-client"] -> do
                            hput ConsolePut on console
                            put "Spawning new client..." on console

                            hput ConsoleStringTerminal on console
                            split console into nconsole, negstrterm

                            plug 
                                negstrterm, strterm => -> negstrterm |=| neg strterm

                                node( | up => racedpasser, nleaf)
                                clientLeaf( [] | nleaf => strterm)

                                server( freshtknum | nconsole, m => up)
                        [":new-ticket", name, desc] -> do
                            split m into timer,mem

                            hput MemGet on mem
                            get memtkts on mem

                            hput MemPut on mem
                            put mkTicket(name, desc, freshtknum) : memtkts on mem

                            hput ConsolePut on console
                            put "Ticket added..." on console

                            memCellLoop( 1 + freshtknum  | console, timer,mem => racedpasser) 

                        -- more or less duplciated from ":new-ticket"
                        [":view" ] -> do
                            split m into timer,mem

                            hput MemGet on mem
                            get memtkts on mem

                            hput ConsolePut on console
                            put "All tickets currently available are as follows..." on console

                            hput ConsolePut on console
                            put showTickets(memtkts) on console

                            memCellLoop( freshtknum  | console, timer,mem => racedpasser) 

                        _ -> do
                            hput ConsolePut on console
                            put "Invalid input, please try again..." on console
                            server( freshtknum | console, m => racedpasser)
                            

                -- duplicated code from 'root' above.
                racedpasser -> do
                    get _ on racedpasser
                    hcase racedpasser of Passer -> fork racedpasser as 
                        rm -> m |=| rm
                        negmandnracedpasser -> do
                            split negmandnracedpasser into negm,nracedpasser
                            plug 
                                serverLoop( freshtknum | console,nm => nracedpasser )
                                => negm, nm -> negm |=| neg nm

    -- | wrapper to help with options that loop with the memory cell 
    proc memCellLoop ::  
        Int
        | Console
        , Timer 
        , MemCell( [Ticket] |) 
        => Get( () | Passer(| Timer (*) MemCell([Ticket]|))) =
            freshtknum | console, timer, mem => racedpasser -> plug
                    mem, timer => nm  -> fork nm as
                        l -> l |=| timer
                        r -> r |=| mem
                    server( freshtknum | console, nm => racedpasser)

-- uhh bnfc apparently does not do the offside rule correctly?
where {
    defn 
        fun pArgs :: [Char] -> [[Char]] = 
            ts -> case skipSpace(ts) of
                [] -> [] 
                nt:nts -> case nt of
                    '"' -> case sliceUntilQuote(nts) of (wrd,nnts) -> wrd : pArgs(nnts)

                    _ -> case sliceUntilSpace(nt:nts) of (wrd,nnts) -> wrd : pArgs(nnts)
    where 
        fun isSpace :: Char -> Bool = 
            ' ' -> True
            '\t' -> True
            '\n' -> True
            _ -> False

        fun skipSpace :: [Char] -> [Char] =
            [] -> []
            t:ts -> if isSpace(t) then skipSpace(ts) else t:ts

        fun sliceUntilSpace :: [Char] -> ([Char], [Char]) =
            [] -> ([], [])
            t:ts -> if isSpace(t) 
                then ([], t:ts)
                else case sliceUntilSpace(ts) of
                    (ls, rs) -> (t:ls, rs)

        fun sliceUntilQuote :: [Char] -> ([Char], [Char]) =
            [] -> ([], [])
            '\\':'"':ts -> case sliceUntilQuote(ts) of (ls, rs) -> ('\\':'"':ls, rs)
            t:ts -> if t ==  '"'
                -- we drop the quote
                then ([], ts)
                else case sliceUntilQuote(ts) of
                    (ls, rs) -> (t:ls, rs)

}

-- | time in seconds for the time out of the clients
fun iNIT_FRESH_TICKET_ID :: -> Int  =
    -> 0

proc run = 
    | timer, console => strterm -> plug
        -- memCell( [mkTicket("Temmie", "A cute white and cream colored pomeranian", 0) ] | => mem  )
        memCell( [] | => mem  )

        timer, mem => m -> fork m as
            l -> l |=| timer
            r -> r |=| mem

        server( iNIT_FRESH_TICKET_ID | console, m => tail)
        clientLeaf( [] | tail => strterm)
