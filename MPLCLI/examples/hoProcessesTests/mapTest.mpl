protocol List(| A) => S =
    Cons :: A (*) S => S
    Nil :: A => S

proc cons :: | T, List( | T) => List( | T) =
    | t, ts => ts' -> do
        hput Cons on ts'
        fork ts' as
            t'' -> t |=| t''
            ts'' -> ts |=| ts''

proc nil :: | T => List(| T) =
    | t => ts -> do
        hput Nil on ts
        ts |=| t



proc mapList :: Store( | A => B) | List(| A) => List(| B) =
    p | la => lb ->
        hcase la of
            Cons -> do
                split la into hla, tla
                plug
                    use(p)(| hla => hlb)
                    fmap(p | tla => tlb)
                    cons(| hlb, tlb => lb)

            Nil -> do
                plug
                    use(p)( | la => b)
                    nil( | b => lb)