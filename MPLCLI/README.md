# MPLCLI

# Bug List
- This does not non exhaustive pattern matches.. 
```
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun append :: [A], [A] -> [A] =
    [], ts -> ts
    s:ss, ts -> s : append(ss,ts)

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

codata S -> Fun(A,B) = 
    App :: A,S -> B

fun map :: Fun(A,B), [A] -> [B] = 
    _, [] -> []
    hfun, t:ts-> App(t,hfun) : map(hfun, ts)

fun concatMap :: Fun(A, [B]), [A] -> [B] =
    hfun,lst -> concat(map(hfun, lst))


defn
    fun transpose :: [[A]] -> [[A]] = 
        [] -> []
        []:ts -> transpose(ts)
where
    fun headstails :: [[A]] -> [([A], [A])] =
        [] -> []
        []:ts -> ([], []) : headstails(ts)
        (s:ss):ts -> ([s], ss) : headstails(ts)

        
    

proc run =
    | _console => -> do

        hput ConsoleClose on _console
        halt _console
```


coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we'd have to fork for that... that's the idea
    ConsoleTerm :: S => S (+) neg(StringTerm)

