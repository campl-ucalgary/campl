{- | Code for racing 2 memory cells which aquire the resource, then release it and any arbitrary time..
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

fun append :: [A],[A] -> [A] =
    [],ts -> ts
    s:ss,ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
	[] -> []
	s:ss -> append(s, concat(ss))


defn
    fun words :: [Char] -> [[Char]] = 
        ts -> case skip(ts) of
            [] -> [] 
            nts -> case slice(nts) of
                (wrd,nnts) -> wrd : words(nnts)
where
    fun isSpace :: Char -> Bool = 
        ' ' -> True
        '\t' -> True
        '\n' -> True
        _ -> False

    fun skip :: [Char] -> [Char] =
        [] -> []
        t:ts -> if isSpace(t) then skip(ts) else t:ts

    fun slice :: [Char] -> ([Char], [Char]) =
        [] -> ([], [])
        t:ts -> if isSpace(t) 
            then ([], t:ts)
            else case slice(ts) of
                (ls, rs) -> (t:ls, rs)

fun unlines :: [[Char]] -> [Char] =
	[] -> []
	s:ss -> append(s, '\n':unlines(ss))

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
coprotocol S => Passer( A | ) =
    Passer :: S => MemCell(A | ) (*) (Neg(MemCell(A | )) (+) Get( () | S)) 

-- | a strTermLeaf in the multi memory cell
proc strTermLeaf :: | Get( () | Passer([Char] | )) => StringTerminal =
    | ch => _strterm -> do
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
            nch -> strTermLeaf(| nch => _strterm)

-- | a node in the list
defn
    proc node :: | Get( () | Passer(A | )) =>  Get( () | Passer(A |)), Get( () | Passer(A | ))  = 
        | up => lch, rch -> race 
            lch -> nodeLoop(|up => lch, rch)
            rch -> nodeLoop(|up => rch, lch)

    proc nodeLoop :: | Get( () | Passer(A | )) =>  Get( () | Passer(A |)), Get( () | Passer(A | ))  = 
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
proc root :: |   MemCell( A | ) => Get( () | Passer(A | ))  =
    | mem  => ch -> do
        get _ on ch
        hcase ch of Passer -> fork ch as 
            rmem -> mem |=| rmem
            negmemandnch -> do
                split negmemandnch into negmem,nch
                plug 
                    root( | nmem => nch )
                    => negmem, nmem -> negmem |=| neg nmem


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
    | => strterm0, strterm1, strterm2 -> do
        plug
            memCell( "dogs!!" | => mem  )
            root( | mem => up1 )
            node(| up1 => up0,ch2  )
            node(| up0 => ch0,ch1  )

            strTermLeaf(| ch0 => strterm0)
            strTermLeaf(| ch1 => strterm1)
            strTermLeaf(| ch2 => strterm2)
-}

defn 
    proc runner :: | => ListP( | StringTerminal) = 
        | => listp -> plug 
            memCell( "dogs!!" | => mem )

            root( | mem => msgs ) 
            unravel( | msgs => listp ) 
where
    proc unravel :: | Get( () | Passer([Char] | )) => ListP( | StringTerminal) = 
        | raceable => listp -> hcase listp of
            ConsP -> do
                split listp into l,r
                plug 
                    raceable => nr, nl -> do node( | raceable => nl, nr )
                    nl => l -> strTermLeaf( | nl => l )
                    nr => r -> unravel( | nr => r)
                
            NilP -> strTermLeaf( | raceable => listp)
    

-- proc run :: | => StringTerminal =
proc run = 
    | => strterm0, strterm1, strterm2 -> plug
        runner(| => listp0)

        consp( | listp0 => strterm0, listp1 )
        consp( | listp1 => strterm1, listp2 )
        nilp(  | listp2 => strterm2)

        {-
        -- In this implementation we use a list of string terminals 
        nilp( | listp => ch )
        strTermLeaf( | ch => strterm)
        -}
