codata S -> Pair(A,B)  =
    P0 ::  S -> A
    P1 ::  S -> B

proc p :: | Put(A | Put (B | X)) => Put(Pair(A, B) | X) =
    | alpha => beta -> do
        get a on alpha
        get b on alpha
        put ( P0 := -> a, P1 := -> b ) on beta
        alpha |=| beta




