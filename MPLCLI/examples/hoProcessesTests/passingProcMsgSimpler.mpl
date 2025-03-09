
proc p :: | => TopBot =
    | => ch -> halt ch

proc server :: | => Put( Store( | => TopBot) | TopBot) =
    | => ch -> do
        put store(p) on ch
        halt ch

proc client :: | Put( Store( | => TopBot) | TopBot) => TopBot =
    | ch1 => ch2 -> do
        get p on ch1
        close ch1
        use(p)( | => ch2)
        
proc run =
    | => ch1 -> plug
        server( | => ch)
        client( | ch => ch1)