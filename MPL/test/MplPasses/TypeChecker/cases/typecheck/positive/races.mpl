proc v27 :: | Put(A | TopBot), Put(A | TopBot) => =
    | a,b => -> do
        race 
            a -> do
                get _ on a
                get _ on b
                close b
                halt a
            b -> do
                get _ on a
                get _ on b
                close a
                halt b

proc v28 :: | => Get(A | TopBot), Get(A | TopBot) =
    |  => a,b -> do
        race 
            a -> do
                get _ on a
                get _ on b
                close b
                halt a
            b -> do
                get _ on a
                get _ on b
                close a
                halt b
