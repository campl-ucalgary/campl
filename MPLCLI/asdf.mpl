protocol Mem(M|) => S =
    MemPut :: Put(M|S) => S
    MemGet :: Get(M|S) => S
    MemCls :: TopBot => S

protocol InpTerm(I|) => S =
    InpPut :: Put(I|S) => S
    InpGet :: Get(I|S) => S
    InpCls :: TopBot => S

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

proc p1 :: | => Passer(|Mem(A|)), InpTerm(A|) = 
    | => passer, inp -> do
        hput Passer on passer
        split passer into mm,nmpp
        hput MemGet on mm 
        get y on mm
        hput InpPut on inp
        put y on inp
        hput InpGet on inp
        get x on inp
        hput MemPut on mm
        put x on mm
        fork nmpp as
            nm with mm -> nm |=| neg mm
            pp with inp -> p1(| => pp, inp)

proc p2 :: | Passer(| Mem(A|)) => InpTerm(A|), Mem(A|) =
    | passer => inp, mem -> do
        hcase passer of
            Passer -> do
                hput MemGet on mem
                get y on mem
                hput InpPut on inp
                put y on inp
                hput InpGet on inp
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

proc run :: | => InpTerm(Int |) , InpTerm(Int|) =
    | => inpterm0, inpterm1 -> do
        plug
            p1(| => passer, inpterm0)
            p2(| passer => inpterm1, mem)
            memory(100 | mem => )

        

