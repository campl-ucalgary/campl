


protocol SomeProtocol => S =
    First :: Get(Int | S) => S
    Second :: TopBot => S

proc a :: | Get(Int | SomeProtocol) => =
    | channel => -> do
        put 0 on channel
        hcase channel of
            First -> a(|channel=>)
            Second -> halt channel

proc b :: | => Get(Int | SomeProtocol) =
    | => channel -> do
        get _ on channel
        hput Second on channel
        halt channel


proc run =
    | => -> do
        plug
            a(|c=>)
            b(|=>c)