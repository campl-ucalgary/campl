protocol Test(A,B | ) => S =
    Testing0 :: Get(B | Put(A | TopBot)) => S 
    Testing1 :: Put(A | TopBot) => S 

proc v12a :: |  => Test(A,A | ) =
    | => other -> do
        hput Testing0 on other
        get a on other
        put a on other
        halt other
