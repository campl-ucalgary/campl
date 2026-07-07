

        
proc p :: Store( | => Get(A | TopBot)) | => Get(A | TopBot) = 
    k | => ch -> use(k)(| => ch)

proc q =
    | => ch -> do
        get _ on ch
        halt ch

proc z =
    | => ch -> p(store(q) | => ch)