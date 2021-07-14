{-
pomeranian
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

codata S -> Monoid(M) =
    MAppend :: M,M,S -> M
    MUnit :: S -> M

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun listMonoid :: -> Monoid([A]) = 
    -> (MAppend := a,b -> append(a,b), MUnit := -> [])

proc run =
    | _console => -> do
        hput ConsolePut on _console
        put MAppend("pome", MAppend(MUnit(listMonoid), "ranian", listMonoid), listMonoid) 
            on _console

        hput ConsoleClose on _console
        halt _console
