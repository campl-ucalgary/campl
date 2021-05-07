proc v30 =
    |  => a -> do
        plug
            => a,c -> do
                close a
                halt c
            c => b -> do
                close b
                halt c
            b => -> do
                halt b
