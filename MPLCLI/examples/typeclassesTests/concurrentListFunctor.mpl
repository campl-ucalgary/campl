-- IT DOES NOT WORK BECAUSE WE CANNOT DUPLICATE CHANNELS

protocol Console => S =
    ConsolePut :: Put( [Char] | S) => S 
    ConsoleGet :: Get( [Char] | S) => S
    ConsoleClose :: TopBot => S

codata S -> Fun(A,B) = 
    App :: A,S -> B

protocol List(| A) => S =
    Cons :: A (*) S => S
    Nil :: A => S

-- class Functor \S -> T( | S) where
--     proc fmap :: | B (+) Neg(A), T( | A) => T( | B)

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

proc apply :: | Neg(A) (+) B, A => B =
    | negAandB, a => b -> do
        fork negAandB as
            nega -> nega |=| neg a
            b' -> b' |=| b

proc fmap :: | Neg(A) (+) B, List( | A) => List( | B) =
    | negAandB, la => lb ->
        hcase la of
            Cons -> do
                split la into hla, tla
                plug
                    apply(| negAandB, hla => hlb)
                    cons(| hlb, tlb => lb)
                    fmap(| negAandB, tla => tlb)

            Nil ->
                plug
                    apply( | negAandB, la => b)
                    nil( | b => lb)


proc putToTopBot :: | Put([Char] | TopBot) => TopBot, Console =
    | ch1 => ch2, console -> do
        hput ConsolePut on console
        put "hello, press any key to start..." on console
        hput ConsoleGet on console
        get key on console
        get inp on ch1
        hput ConsolePut on console
        put inp on console
        hput ConsolePut on console
        put "channel can be killed now!" on console
        hput ConsoleClose on console
        close console
        ch1 |=| ch2
        

proc splitter :: | A, B => A (+) B = 
    | t1, t2 => t3 -> do
        split t3 into t11, t22
        plug
            z, t1 => t11 -> do
                close z
                t1 |=| t11
            t2 => t22, z -> do
                close z
                t2 |=| t22

proc createList = 
    | a, b, c, d => ldcba ->
        plug
            nil( | a => la)
            cons( | b, la => lba)
            cons( | c, lba => lcba)
            cons( | d, lcba => ldcba)


proc run :: | Get([Char] | Put([Char] | TopBot)), 
            Get([Char] | Put([Char] | TopBot)), 
            Get([Char] | Put([Char] | TopBot)), 
            Get([Char] | Put([Char] | TopBot)) => List(|TopBot), Console =
    | a, b, c, d => idldcba, console -> do
        put "a" on a
        put "b" on b
        put "c" on c
        put "d" on d
        plug
            createList( | a, b, c, d => ldcba)
            fmap( | negxAndidx, ldcba => idldcba)
            putToTopBot( | x => idx, console )
            splitter( | negx, idx => negxAndidx)
            => x, negx -> negx |=| neg x


