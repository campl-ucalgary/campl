protocol Test(A,B | ) => S =
    Testing0 :: Get(A | TopBot) => S
    Testing1 :: Get(A | TopBot) => S

proc v19 :: | => Test(A,B | ) =
    | => other -> do
        hput Testing0 on other
        get _ on other
        halt other
