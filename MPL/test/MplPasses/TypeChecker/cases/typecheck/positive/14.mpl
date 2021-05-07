proc v14 :: | => Get(A|Put(A|TopBot)) =
    | => b -> do
        get a on b 
        put a on b
        halt b
