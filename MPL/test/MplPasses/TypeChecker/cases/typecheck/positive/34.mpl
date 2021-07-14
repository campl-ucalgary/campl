
proc v35 =
    | a => -> do
        plug
            c =>  -> do
                halt c
            a => b,c -> do
                close a
                close b
                halt c
            b =>  -> do
                halt b
