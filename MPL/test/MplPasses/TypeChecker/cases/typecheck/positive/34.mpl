proc v34 =
    | a => -> do
        plug
            f => -> do
                halt f
            a => b,c -> do
                close a 
                close b 
                halt c
            b,c => d,e,f -> do
                close b 
                close c
                close d
                close e
                halt f
            e,d => -> do
                close d
                halt e
