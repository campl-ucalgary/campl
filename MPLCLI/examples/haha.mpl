
protocol IntTerminal => C =
    IntTerminalGet :: Get([Char] | C) => C
    IntTerminalPut :: Put([Char] | C) => C
    IntTerminalClose :: TopBot => C

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

proc p1 :: | => Passer(|Mem([Char]|)), IntTerminal = 
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
            nm with mm -> nm |=| neg mm
            pp with _inp -> p1(| => pp, _inp)

proc p2 :: | Passer(| Mem([Char]|)) => IntTerminal, Mem([Char]|) =
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
                    mm with mem -> do
                        mm |=| mem
                    nmpp with _inp -> do
                        split nmpp into nm, pp
                        plug
                            p2( | pp => _inp,z)
                            z,nm => -> nm |=| neg z

proc run :: | => IntTerminal , IntTerminal =
    | => _inpterm0, _inpterm1 -> do
        plug 
            p1(|        => passer, _inpterm0)
            p2(| passer => _inpterm1, mem   )
            memory("a" | mem => )
