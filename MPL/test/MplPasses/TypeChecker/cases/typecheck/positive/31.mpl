proc v31 =
    |  => -> do
        plug
            f => -> do
                halt f
            => f,a -> do
                close a
                halt f
            a => -> do
                halt a
