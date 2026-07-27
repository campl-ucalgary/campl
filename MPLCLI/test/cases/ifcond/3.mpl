{-
else
-}

-- Regression test: a `case` command inside the else branch of a
-- process-level `if` floats a compiler-generated process definition.
-- Previously the pattern compiler dropped the else branch's floated
-- definitions (and emitted the condition's twice), so this program
-- failed to compile with a spurious out-of-scope process error.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

proc run :: | Console => =
    | _console => -> do
        if 1 < 0
            then do
                hput ConsolePut on _console
                put "then" on _console
                hput ConsoleClose on _console
                halt _console
            else do
                case "else" of
                    msg -> do
                        hput ConsolePut on _console
                        put msg on _console
                        hput ConsoleClose on _console
                        halt _console
