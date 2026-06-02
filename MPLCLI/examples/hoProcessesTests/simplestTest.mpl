proc p :: Store( | A => Put(X | A)) | A => Put(X | A) =
    q | ch0 => ch1 -> use(q)( | ch0 => ch1)


proc z :: X, X | A => Put(X | Put(X | A)) =
    x, y | ch0 => ch1 -> do
            put y on ch1
            p(store( | a => b -> do
                                put x on b
                                a |=| b
                    ) | ch0 => ch1)

proc run =
    | x => -> halt x