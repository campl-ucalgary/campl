proc v33 =
    |  => -> do
        plug
            a => -> do
                halt a
            => f,a -> do
                close a
                halt f
            f => -> do
                halt f
