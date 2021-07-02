{-
abcdefg
-}

fun append :: [A],[A] -> [A] =
    s:ss, ts -> s : append(ss,ts)
    [], ts -> ts

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put concat(["ab","cd","e","","f","g"]) on _console
        hput ConsoleClose on _console
        halt _console

