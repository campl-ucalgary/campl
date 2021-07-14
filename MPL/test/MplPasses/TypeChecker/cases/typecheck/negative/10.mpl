-- bad plug
proc n35 =
    | a => -> do
        plug
            b,c => d,e,f -> do
                close b 
                close c
                close d
                close e
                halt f
            a => b,c -> do
                close a 
                close b 
                halt c
            e,d => -> do
                close d
                halt e
            f => -> do
                halt f
