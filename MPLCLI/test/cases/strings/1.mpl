{-
a
b
back\slash "quote"
-}

-- String literals: the lexer admits the escapes \n \t \r \f \\ \"
-- inside a string token, and the parser must unescape all of them.
-- The unescaper used to crash with "impossible bnfc error happened"
-- on \\ and \".

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "a\nb" on _console
        hput ConsolePut on _console
        put "back\\slash \"quote\"" on _console
        hput ConsoleClose on _console
        halt _console
