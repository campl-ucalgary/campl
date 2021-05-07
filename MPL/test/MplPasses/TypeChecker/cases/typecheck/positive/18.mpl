protocol Test(A,B | ) => S =
    Testing0 :: Put(A | Get(B |TopBot)) => S
    Testing1 :: Put(B | TopBot) => S

proc v18 :: | Test(A,A |) => = 
    | a => -> do
        hcase a of
            Testing0 -> do
                get res on a
                put res on a
                halt a
            Testing1 -> do
                get _ on a
                halt a
