{-
custom append
-}

-- A user definition of (++) anywhere in the program takes
-- precedence over the built-in list append: no definition is
-- injected and every use calls the user's version, at whatever
-- type the user gave it.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun (++) :: [Char],[Char] -> [Char] =
    a,b -> "custom append"

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "x" ++ "y" on _console
        hput ConsoleClose on _console
        halt _console
