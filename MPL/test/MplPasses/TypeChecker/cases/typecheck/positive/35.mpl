
proc v35 =
    | a => -> do
        plug
            a => b,c -> do
                close a
                close b
                halt c
            b =>  -> do
                halt b
            c =>  -> do
                halt c

