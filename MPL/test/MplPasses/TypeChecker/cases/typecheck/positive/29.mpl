proc v29 :: | => TopBot =
    |  => a -> do
        plug
            => a,c -> do
                close a
                halt c
            c => -> do
                halt c
