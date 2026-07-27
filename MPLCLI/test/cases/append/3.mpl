{-
ab3
-}

-- The injected (++) is polymorphic list append, not just string
-- concatenation: here it appends [Char] lists built from list
-- syntax and a pattern match consumes the result, and it is also
-- used inside a function definition (the injected definition is
-- visible program-wide, wherever the use appears).

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun three :: [Int] -> [Char] =
    x:y:z:[] -> "3"
    _ -> "not 3"

fun label :: [Char] -> [Char] =
    s -> s ++ three([1] ++ [2,3])

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put label(['a'] ++ ['b']) on _console
        hput ConsoleClose on _console
        halt _console
