protocol Test => S =
    Testing0 :: TopBot => S
    Testing1 :: TopBot => S

proc v17 :: | Test( | ) => TopBot =
    | a => other -> do
        hcase a of
            Testing0 -> do
                close other
                halt a
            Testing1 -> do
                close other
                halt a
