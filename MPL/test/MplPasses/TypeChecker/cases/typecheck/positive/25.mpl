proc v25 :: | => TopBot (*) TopBot, TopBot =
    | => a,other -> do
        fork a as
            s -> do
                close other 
                halt s
            t -> halt t
