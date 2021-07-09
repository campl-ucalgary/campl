{-
banana
bananana
banananana
bananananana
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun append :: [A], [A] -> [A] = 
    [],ts -> ts
    s:ss,ts -> s : append(ss, ts) 

codata S -> Stream(A) =
    SHead :: S -> A
    STail :: S -> S

fun banana :: -> Stream([Char])  =
    -> 
        ( SHead := -> "banana"
        , STail := -> 
            let fun stail :: [Char] -> Stream([Char]) =
                    str -> 
                        let fun tmp = -> append(str, "na")
                        in
                            ( SHead := -> tmp 
                            , STail := -> stail(tmp)
                            )
            in stail("banana")
        )


proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put SHead(banana) on _console

        hput ConsolePut on _console
        put SHead(STail(banana)) on _console

        hput ConsolePut on _console
        put SHead(STail(STail(banana))) on _console

        hput ConsolePut on _console
        put SHead(STail(STail(STail(banana)))) on _console

        hput ConsoleClose on _console
        halt _console


