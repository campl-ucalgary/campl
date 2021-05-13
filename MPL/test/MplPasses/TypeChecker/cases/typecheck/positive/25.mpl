{-
proc v25 :: | => TopBot (*) TopBot, TopBot =
    | => a,other -> do
        fork a as
            s -> do
                close other 
                halt s
            t -> halt t

proc v26 :: | TopBot (+) TopBot, TopBot =>  =
    | a,other =>  -> do
        fork a as
            s -> do
                close other 
                halt s
            t -> halt t
-}
