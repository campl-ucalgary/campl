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

{-
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

-}

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


{-
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
-}


{-
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

-}

{-
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p1 =
    | => a -> do
        split a into a0,a1
        put 0 on a0
        put 1 on a1
        close a0
        halt a1
        

proc p2 =
    | a => _intterm0, _intterm1 -> do
        fork a as
            a0 -> do
                get n on a0

                hput IntTerminalPut on _intterm0
                put n on _intterm0

                hput IntTerminalClose on _intterm0
                close _intterm0

                halt a0
            a1 -> do
                get n on a1

                hput IntTerminalPut on _intterm1
                put n on _intterm1

                hput IntTerminalClose on _intterm1
                close _intterm1

                halt a1

proc run =
    | => _intterm0, _intterm1 ->  do
        plug
            p1( |     => a)
            p2( |  a  => _intterm0,  _intterm1)
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 


-- oopsies
proc p0 :: | Get([Char] | TopBot) =>  =
    | ch0  => -> do
        put "pomeranian" on ch0
        halt ch0

proc p1 :: | => A, Neg(A) =
    | => ch0, ch1 -> do
        ch0 |=| neg ch1

proc p2 :: | A, Neg(A) => =
    | ch1, ch2 =>  -> do
        ch1 |=| neg ch2

proc p3 :: | Console => Get([Char] | TopBot) =
    | _console => ch  -> do
        get n on ch 
        close ch

        hput ConsolePut on _console
        put n on _console

        hput ConsoleClose on _console
        halt _console

proc run =
    | _console => -> do
        plug 
            p0( | ch0      =>          )
            p1( |          => ch0, ch1 )
            p2( | ch1, ch2 =>          )
            p3( | _console => ch2      )


{-
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc p0 :: |  => Put([Char] | Put([Char] | TopBot)) =
    | => ch0 -> do
        put "yorkshire terrier" on ch0
        put "pomeranian" on ch0
        halt ch0

proc p1 :: | Put([Char] | P ) => P =
    | ch0 => ch1 -> do
        get n on ch0
        ch0 |=| ch1

proc p2 :: | Console, Put([Char] | TopBot) => =
    | _console, ch1 =>  -> do
        get n on ch1
        close ch1

        hput ConsolePut on _console
        put n on _console

        hput ConsoleClose on _console
        halt _console

proc run =
    | _console => ->  do
        plug
            p0(|                => ch0 )
            p1(|           ch0  => ch1 )
            p2(| _console, ch1  =>     )
-}


{-
protocol InfGetPut => S =
    InfGetPut :: Get([Char] | S) => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc infgetput :: Char, [Char] |InfGetPut => =
    tocons, str | ch  => -> hcase ch of 
        InfGetPut -> case tocons:str of
            res -> do
                put res on ch
                infgetput(tocons, res | ch => )
        

proc p0 :: | Console => InfGetPut = 
    | _console => ch -> do
        hput InfGetPut on ch
        get n on ch

        hput ConsolePut on _console

        p0( | _console => ch )


proc run =
    | _console => ->  do
        plug 
            infgetput('a', "" | ch => )
            p0( | _console => ch  )
-}
