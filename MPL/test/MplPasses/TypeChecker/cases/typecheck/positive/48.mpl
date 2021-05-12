-- memory cell example
protocol Mem(A|) => P =
    Put :: Put(A|P) => P
    Get :: Get(A|P) => P
    Cls :: TopBot => P

protocol Passer(|A) => P =
    Pass :: A (+) (Neg(A) (*) P) => P

proc memory :: A | Mem(A|) => =
    x | ch => -> do
        hcase ch of
            Put -> do
                get y on ch
                memory(y | ch => )
            Get -> do
                put x on ch
                memory(x | ch => )
            Cls -> do
                halt ch
