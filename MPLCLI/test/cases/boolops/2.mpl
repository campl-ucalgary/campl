{-
custom or
custom and
-}

-- A user definition of (||) or (&&) anywhere in the program takes
-- precedence over the built-in boolean meaning, so existing
-- programs that define these operators keep working -- at any type.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun (||) :: [Char],[Char] -> [Char] =
    a,b -> "custom or"

fun (&&) :: [Char],[Char] -> [Char] =
    a,b -> "custom and"

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "x" || "y" on _console
        hput ConsolePut on _console
        put "x" && "y" on _console
        hput ConsoleClose on _console
        halt _console
