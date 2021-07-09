{-
Nothing
Just(a)
Nothing
Just(bc)
Nothing
Just(c)
Nothing
Just(ab)
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

data Maybe(A) -> S =
    Just    :: A -> S
    Nothing ::   -> S

fun head :: [A] -> Maybe(A) =
    [] -> Nothing
    s:_ -> Just(s)

fun tail :: [A] -> Maybe([A]) =
    [] -> Nothing
    _:ss -> Just(ss)


fun last :: [A] -> Maybe(A) =
    [] -> Nothing
    [s] -> Just(s)
    _:ss -> last(ss)

fun init :: [A] -> Maybe([A]) =
    [] -> Nothing
    [s] -> Just([])
    s:ss -> Just(
            case init(ss) of
                Just(nss) -> s:nss
                Nothing -> [s]
        )


fun append :: [A],[A] -> [A] =
    s:ss, ts -> s : append(ss,ts)
    [], ts -> ts

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

fun showMaybeChar :: Maybe(Char) -> [Char] =
    Nothing -> "Nothing"
    Just(c) -> concat(["Just(", [c] ,")"])

fun showMaybeStr :: Maybe([Char]) -> [Char] =
    Nothing -> "Nothing"
    Just(str) -> concat(["Just(", str ,")"])


proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put showMaybeChar(head([])) on _console
        hput ConsolePut on _console
        put showMaybeChar(head("abc")) on _console

        hput ConsolePut on _console
        put showMaybeStr(tail([])) on _console
        hput ConsolePut on _console
        put showMaybeStr(tail("abc")) on _console

        hput ConsolePut on _console
        put showMaybeChar(last([])) on _console
        hput ConsolePut on _console
        put showMaybeChar(last("abc")) on _console

        hput ConsolePut on _console
        put showMaybeStr(init([])) on _console
        hput ConsolePut on _console
        put showMaybeStr(init("abc")) on _console

        hput ConsoleClose on _console
        halt _console
