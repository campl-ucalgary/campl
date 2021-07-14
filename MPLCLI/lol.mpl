


fun testing :: Int -> Bool =
    _ -> True


proc testproc =
    a | => ch -> if testing(a) 
        then halt ch
        else halt ch
        

