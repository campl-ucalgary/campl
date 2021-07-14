
protocol IntTerminal => S =
    IntTerminalPut :: Put( Int | S) => S
    IntTerminalGet :: Get( Int | S) => S 
    IntTerminalClose :: TopBot => S

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
    | => passer, _inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput IntTerminalPut on _inp
        put y on _inp
        hput IntTerminalGet on _inp
        get x on _inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm -> nm |=| neg mm
            pp -> p1(| => pp, _inp)

proc p2 :: | Passer(| Mem(Int|)) => IntTerminal, Mem(Int|) =
    | passer => _inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput IntTerminalPut on _inp
                put y on _inp
                hput IntTerminalGet on _inp
                get x on _inp
                hput MemPut on mem
                put x on mem
                fork passer as
                    mm -> do
                        mm |=| mem
                    nmpp -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => _inp,z)
                            z,nm => -> nm |=| neg z 

proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(| => passer, _inpterm0)
            p2(| passer => _inpterm1, mem)
            memory(100 | mem => )


