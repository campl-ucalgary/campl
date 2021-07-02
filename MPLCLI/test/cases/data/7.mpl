{-
Zero,Succ(Zero),
Zero,Succ(Zero),Zero,Succ(Succ(Zero)),
-}

data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

data Tree(A) -> S=
    Bin :: S,A,S -> S
    Leaf ::      -> S

fun showBool :: Bool -> [Char] =
    True -> "True"
    False -> "False"

fun append :: [A],[A] -> [A] =
    s:ss, ts -> s : append(ss,ts)
    [], ts -> ts

fun concat :: [[A]] -> [A] =
    [] -> []
    s:ss -> append(s, concat(ss))

fun showNat :: Nat -> [Char] =
    Zero -> "Zero"
    Succ(n) -> concat(["Succ(", showNat(n), ")"])


fun collectNats :: Tree(Nat) -> [Nat] =
    Bin(l, s, r) -> concat([collectNats(l), [s] ,collectNats(r)])
    Leaf -> []

fun showNatList :: [Nat] -> [Char] =
    s:ss -> concat([showNat(s), "," , showNatList(ss)])
    [] -> []


coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun testTree :: -> Tree(Nat) =
    -> Bin
        ( Bin
            ( Leaf
            , Zero
            , Leaf
            )
        , Succ(Zero)
        , Bin
            ( Leaf
            , Zero
            , Bin
                ( Leaf
                , Succ(Succ(Zero))
                , Leaf
                )
            )
        )
    

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put showNatList(collectNats(Bin(Bin(Leaf,Zero,Leaf), Succ(Zero), Leaf))) on _console
        hput ConsolePut on _console
        put showNatList(collectNats(testTree)) on _console
        hput ConsoleClose on _console
        halt _console
