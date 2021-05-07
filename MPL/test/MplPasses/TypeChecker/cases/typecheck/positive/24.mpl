
proc v24 :: | => Get(A | TopBot) (+) TopBot =
    | => a -> do
        split a into s,t
        get v on s
        close s 
        halt t
