{-
Nothing
Just((a, bc))
a
b
True
False
0
1
2
3
4
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

data Maybe(A) -> S =
    Just    :: A -> S
    Nothing ::   -> S

fun uncons :: [A] -> Maybe((A,[A])) =
    [] -> Nothing
    s:ss -> Just( (s, ss) )

fun singleton :: A -> [A] =
    s -> [s]

fun null :: [A] -> Bool =
    [] -> True
    _ -> False

fun length :: [A] -> Int =
    [] -> 0
    _ : ss -> 1 + length(ss)

fun append :: [A],[A] -> [A] =
    s:ss, ts -> s : append(ss,ts)
    [], ts -> ts

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

fun showUnconsResult :: Maybe( (Char,[Char]) ) -> [Char] =
    Nothing -> "Nothing"
    Just((c,cs)) -> concat(["Just((", [c] ,", ", cs ,"))"])

fun showBool :: Bool -> [Char] =
    True -> "True"
    False -> "False"


-- this is for showing an int
defn
    fun showInt :: Int -> [Char] =
        0 -> ['0']
        n -> go(n, [])
where
    -- this does integer division for a,b as input
    -- and outputs (q,r) where q is the integer division and r 
    -- is the remainder.
    fun divqr :: Int, Int -> (Int, Int) =
        a, b -> if a < b
            then (0, a)
            else case divqr(a - b, b) of
                (q, r) ->  (q + 1, r)

    fun intToDigit :: Int -> Char =
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        -- in the use cases of this progrma, this will never happen!
        _ -> '#'

    fun go :: Int, [Char] -> [Char] =
        0, acc -> acc
        n, acc -> case divqr(n, 10) of
            (q, r) -> go(q, intToDigit(r):acc)

proc run :: | Console => =
    | _console => -> do
        -- uncons
        hput ConsolePut on _console
        put showUnconsResult(uncons([])) on _console
        hput ConsolePut on _console
        put showUnconsResult(uncons("abc")) on _console

        -- singleton
        hput ConsolePut on _console
        put singleton('a') on _console
        hput ConsolePut on _console
        put singleton('b') on _console

        -- null
        hput ConsolePut on _console
        put showBool(null([])) on _console
        hput ConsolePut on _console
        put showBool(null([1,2,3])) on _console

        -- length
        hput ConsolePut on _console
        put showInt(length([])) on _console
        hput ConsolePut on _console
        put showInt(length([1])) on _console
        hput ConsolePut on _console
        put showInt(length([1,2])) on _console
        hput ConsolePut on _console
        put showInt(length([1,2,3])) on _console
        hput ConsolePut on _console
        put showInt(length([1,2,3,4])) on _console

        hput ConsoleClose on _console
        halt _console
