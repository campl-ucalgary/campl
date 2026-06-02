
protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

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
        
protocol State(| A, S) => T =
    State :: Neg(S) (+) (A (*) S) => T

proc runState :: | State(| A, S), S => A (*) S =
    | st, s => as' -> do
        hcase st of
            State -> do
                fork st as
                    negS -> negS |=| neg s
                    aAndS -> as' |=| aAndS

proc pmap :: | Neg(A) (+) B, State(| A, S) => State(| B, S) =
    | negAandB, sta => stb -> do
        hput State on stb
        split stb into negstb, bandsb
        hcase sta of
            State -> do
                fork sta as
                    negsta -> negsta |=| negstb
                    aaAndSA -> do
                        split aaAndSA into aa, sa
                        fork bandsb as 
                            b -> do
                                    fork negAandB as
                                        negA -> negA |=| neg aa
                                        bb -> b |=| bb
                            sb -> sa |=| sb


proc return :: | A => State( | A, S) =
    | a => sm -> do
        hput State on sm
        split sm into negs, aands
        fork aands as
            a' -> a |=| a'
            s -> negs |=| neg s

proc bind :: | State( | A, S), Neg(A) (+) State( | B, S) => State( | B, S) = 
    | sta, negAandStb => stb -> do
        hput State on stb
        split stb into negsb, bandsb
        plug
            => sb', negsb -> negsb |=| neg sb'
            runState( | sta, sb' => aands')
            negAandStb, aands' => bandsb -> do
                split aands' into a', s'
                fork negAandStb as
                    nega -> nega |=| neg a'
                    stb -> plug
                        runState( | stb, s' => bandsb')
                        bandsb' => bandsb -> bandsb' |=| bandsb

proc pget :: | => State( | S, S) =
    | => st -> do
        hput State on st
        split st into negs, ss
        fork ss as
            s1 -> negs |=| neg s1
            s2 -> pput( | s2)

proc pput :: | S => State( | S, S) =
    | sIn => sts -> do
        hput State on sts
        split sts into negs, ss
        fork ss as
            s1 -> negs |=| neg s1
            s2 -> s2 |=| sIn

proc p1 :: | MemCell([Char] |) => State(| MemCell([Char]), S) =
    | memIn => st -> do
        hput MemGet on memIn
        get inp on memIn
        hput MemPut on memIn
        put "hello from p1" on memIn   
        return ( | memIn => st)

proc p2 :: | MemCell([Char] |) => State(| MemCell([Char]), S) =
    | memIn => st -> do
        hput MemGet on memIn
        get inp on memIn
        hput MemPut on memIn
        put "hello from p2" on memIn   
        return ( | memIn => st)


proc splitter :: | A, B => A (+) B = 
    | t1, t2 => t3 -> do
        split t3 into t11, t22
        plug
            z, t1 => t11 -> do
                close z
                t1 |=| t11
            t2 => t22, z -> do
                close z
                t2 |=| t22

proc stateMachine :: | => State( | MemCell([Char] | ), MemCell([Char] |)) =
    | => st -> plug
        pget( | => currState)
        p1( | mem1 => stm1)
        => negmem1, mem1 -> negmem1 |=| neg mem1
        splitter(| stm1, negmem1 => hp1)       
        bind( | currState, hp1 => nextState)
        p2( | mem2 => stm2)
        => negmem2, mem2 -> negmem2 |=| neg mem2
        splitter(| stm2, negmem2 => hp2)
        bind( | nextState, hp2 => nextState')
        return( | nextState' => st)

proc showMem :: | MemCell([Char] |) => StringTerminal =
    | mem => strterm -> do
        hput MemGet on mem
        get inp on mem
        hput StringTerminalPut on strterm
        put inp on strterm
        hput StringTerminalGet on strterm
        get _ on strterm
        close mem
        halt strterm

proc run =
    | => strterm1, strterm2 -> 
        plug
            stateMachine(| => statem)
            memCell("hello" | => mem)
            runState(| statem, mem => out)
            out => strterm1, strterm2 -> do
                fork out as
                    a -> showMem(| a => strterm1)
                    s -> showMem(| s => strterm2)

