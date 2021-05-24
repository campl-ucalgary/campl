proc procswitch :: Bool,Bool | TopBot => TopBot  = 
    somebool0, somebool1,| a => b ->
        switch
            somebool0 -> do
                close a
                halt b
            somebool1 -> do
                close a
                halt b

proc procif :: Bool | TopBot => TopBot = 
    somebool | a => b ->
        if somebool
            then do
                close a
                halt b
            else do
                close a
                halt b
