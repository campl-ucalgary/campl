proc v30 :: | => A =
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
