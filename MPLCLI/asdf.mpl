protocol IntTerminal => C =
    IntTerminalGet :: Get(Int | C) => C
    IntTerminalPut :: Put(Int | C) => C
    IntTerminalClose :: TopBot => C

{-
proc p1 =
    | => a -> do
        split a into a0,a1
        put 0 on a0
        put 1 on a1
        close a0
        halt a1
        

proc p2 =
    | a => intterm0, intterm1 -> do
        fork a as
            a0 -> do
                get n on a0

                hput IntTerminalPut on intterm0
                put n on intterm0

                hput IntTerminalClose on intterm0
                close intterm0

                halt a0
            a1 -> do
                get n on a1

                hput IntTerminalPut on intterm1
                put n on intterm1

                hput IntTerminalClose on intterm1
                close intterm1

                halt a1
-}

{-
proc run =
    | => _intterm0, _intterm1 ->  do
        plug
            p1( |     => a)
            p2( |  a  => _intterm0,  _intterm1)
-}

{-
proc p1 =
    | a => intterm -> do

        a |=| intterm

proc p2 =
    | => a, intterm -> do
        hput IntTerminalGet on a
        get v on a
        hput IntTerminalClose on a
        close a

        hput IntTerminalPut on intterm
        put v on intterm
        hput IntTerminalClose on intterm
        halt intterm

proc run =
    | => _intterm0, _intterm1 -> do
        plug
            p1( | a =>  _intterm0 )
            p2( |   => a, _intterm1 )
-}

{-
protocol IntTerminal => C =
    IntTerminalGet :: Get(Int | C) => C
    IntTerminalPut :: Put(Int | C) => C
    IntTerminalClose :: TopBot => C

proc p1 =
    | => a , intterm -> do
        hput IntTerminalGet on intterm
        get val on intterm
        hput IntTerminalClose on intterm
        close intterm

        put val on a
        halt a

proc p2 =
    | intterm => fkkkk -> do
        get inp on intterm
        close intterm

        hput IntTerminalPut on fkkkk
        put inp on fkkkk
        hput IntTerminalClose on fkkkk
        halt fkkkk


fun testing =
    a,b -> a

proc run =
    | => _intterm0, _intterm1 -> do
        plug
            p1( | => a, _intterm0 )
            p2( | a => _intterm1 )
-}

{-
proc run =
    | => _intterm -> do
        hput IntTerminalGet on _intterm
        get n on _intterm
        hput IntTerminalPut on _intterm
        put n + 1 on _intterm
        -- hput IntTerminalClose on _intterm
        run( | => _intterm )
-}

{-
proc fk =
    | a,b => -> do
        race
            a -> do
                get _ on a
                get _ on b
                close a
                halt b
            b -> do
                get _ on a
                get _ on b
                close a
                halt b
-}

{-
-- MEMORY CELL: This is broken. 
protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol Passer(|P) => S =
    Passer :: P (+) (Neg(P) (*) S) => S

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            MemPut -> do
                get y on ch
                memory(y | ch => )
            MemGet -> do
                put x on ch
                memory(x | ch => )
            MemCls -> do
                halt ch

proc p1 :: | => Passer(|Mem(Int|)), IntTerminal = 
    | => passer, inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput IntTerminalPut on inp
        put y on inp
        hput IntTerminalGet on inp
        get x on inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm with mm -> nm |=| neg mm
            pp with inp -> p1(| => pp, inp)

proc p2 :: | Passer(| Mem(Int|)) => IntTerminal, Mem(Int|) =
    | passer => inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput IntTerminalPut on inp
                put y on inp
                hput IntTerminalGet on inp
                get x on inp
                hput MemPut on mem
                put x on mem
                fork passer as
                    mm with mem -> do
                        mm |=| mem
                    nmpp with inp -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => inp,z)
                            z,nm => -> z |=| neg nm

proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )
-}


data
    MyBool -> S =
        MyTrue :: -> S
        MyFalse :: -> S
        

fun intToMyBool :: Int -> MyBool =
    0 -> MyFalse
    _ -> MyTrue

fun myBoolToInt :: MyBool -> Int =
    MyFalse -> 0
    MyTrue -> 1


{-
-- trivial call convention test.
proc run :: | => IntTerminal, IntTerminal =
    | => _intterm1, _intterm2 -> do
        hput IntTerminalGet on _intterm1
        get n on _intterm1
        hput IntTerminalClose on _intterm1
        close _intterm1

        hput IntTerminalPut on _intterm2
        put myBoolToInt(intToMyBool(n)) on _intterm2
        hput IntTerminalClose on _intterm2

        halt _intterm2
-}



-- RACE PARALLEL OR
-- gets the service information 
proc p1serviceor :: | => IntTerminal, IntTerminal, Put (MyBool | TopBot) (*) Put (MyBool | TopBot) =
    | => intterm1, intterm2, ch -> do
        fork ch as
            ch1 with intterm1 -> do
                hput IntTerminalGet on intterm1
                get x on intterm1
                put intToMyBool(x) on ch1
                close ch1
                hput IntTerminalClose on intterm1
                halt intterm1
            ch2 with intterm2 -> do
                hput IntTerminalGet on intterm2
                get x on intterm2
                put intToMyBool(x) on ch2
                close ch2
                hput IntTerminalClose on intterm2
                halt intterm2
        


proc p2serviceor :: | Put (MyBool | TopBot) (*) Put (MyBool | TopBot) => IntTerminal = 
    | ch =>  intterm3 -> do
        split ch into ch1, ch2
        race 
            ch1 -> do
                get va on ch1
                case va of
                    MyTrue -> do
                        hput IntTerminalPut on intterm3
                        put myBoolToInt(va) on intterm3
                        hput IntTerminalClose on intterm3
                        close intterm3
                        get _ on ch2

                        close ch1
                        halt ch2
                    MyFalse -> do
                        get vb on ch2

                        hput IntTerminalPut on intterm3
                        put myBoolToInt(vb) on intterm3
                        hput IntTerminalClose on intterm3

                        close intterm3

                        close ch1
                        halt ch2

            ch2 -> do
                get va on ch2
                case va of
                    MyTrue -> do
                        hput IntTerminalPut on intterm3
                        put myBoolToInt(va) on intterm3
                        hput IntTerminalClose on intterm3
                        close intterm3
                        get _ on ch1

                        close ch2
                        halt ch1
                    MyFalse -> do
                        get vb on ch1

                        hput IntTerminalPut on intterm3
                        put myBoolToInt(vb) on intterm3
                        hput IntTerminalClose on intterm3

                        close intterm3
                        close ch2
                        halt ch1

proc run :: | => IntTerminal, IntTerminal, IntTerminal =
    | => _intterm1, _intterm2, _intterm3 -> do
        plug 
            p1serviceor(| => _intterm1, _intterm2, ch)
            p2serviceor(| ch => _intterm3)

