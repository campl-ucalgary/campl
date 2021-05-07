protocol Test => S =
    -- Testing :: TopBot => S
    Testing :: TopBot => S

proc v16 =
    | a => -> do
        hcase a of
            Testing -> halt a
