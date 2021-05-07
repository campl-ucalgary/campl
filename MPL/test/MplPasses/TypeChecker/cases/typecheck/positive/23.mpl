protocol Test(A| ) => S =
    Testing0 :: Get(A | S) => S 

proc v23 :: |  => Test(A | ) =
    | => other -> do
        hput Testing0 on other
        get a on other
        v23(| => other )
