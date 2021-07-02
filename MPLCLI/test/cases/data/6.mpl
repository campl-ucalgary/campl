{-
True
True
False
-}

data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

-- less than or equal to for natural numbers. 
fun natLe :: Nat,Nat -> Bool =
    Zero, _ -> True
    Succ(_), Zero -> False
    Succ(a0), Succ(a1) -> natLe(a0,a1)

fun showBool :: Bool -> [Char] =
    True -> "True"
    False -> "False"



coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put showBool(natLe(Zero,Zero)) on _console
        hput ConsolePut on _console
        put showBool(natLe(Succ(Zero),Succ(Zero))) on _console
        hput ConsolePut on _console
        put showBool(natLe(Succ(Succ(Zero)),Succ(Zero))) on _console
        hput ConsoleClose on _console
        halt _console

