{-
matched
-}

-- String patterns must be unescaped exactly like string literals.
-- Previously a pattern kept its escapes verbatim, so "a\nb" below
-- compared against backslash-n and could never match the literal.
-- The match lives in a function (not a proc-level case) so this
-- test is independent of the proc-level-if floated-defns fix.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun check :: [Char] -> [Char] =
    "a\nb" -> "matched"
    _ -> "wrong branch"

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put check("a\nb") on _console
        hput ConsoleClose on _console
        halt _console
