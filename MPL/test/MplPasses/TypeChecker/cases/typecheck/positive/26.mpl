proc v26 :: | Put(A| B) => B =
    | a => b -> do
        get _ on a
        a |=| b
