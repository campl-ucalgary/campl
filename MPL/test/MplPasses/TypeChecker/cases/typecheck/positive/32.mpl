proc v32 =
    |  => -> do
        plug
            => f,a -> do
                close a
                halt f
            f => -> do
                halt f
            a => -> do
                halt a
