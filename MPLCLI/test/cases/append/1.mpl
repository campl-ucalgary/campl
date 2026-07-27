{-
abcdef
xyz
abc
one two three
-}

-- Built-in ++ (list append): when a program uses ++ without
-- defining it, the compiler injects the standard recursive
-- definition (MplPasses.Parser.ResolveBuiltinOps). Covers appending
-- to an empty and a non-empty list, the sectioned form (++)(_, _),
-- and left-nested chains (++ is left-associative at precedence 5).

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "abc" ++ "def" on _console
        hput ConsolePut on _console
        put "" ++ "xyz" on _console
        hput ConsolePut on _console
        put (++)("abc", "") on _console
        hput ConsolePut on _console
        put "one" ++ " " ++ "two" ++ " " ++ "three" on _console
        hput ConsoleClose on _console
        halt _console
