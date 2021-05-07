proc v15 :: | Put(A|Get(A|TopBot)) =>  =
    | b => -> do
        get a on b 
        put a on b
        halt b
