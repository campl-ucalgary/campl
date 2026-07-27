{-
short and
short or
-}

-- || and && are short-circuiting: the right operand must not be
-- evaluated when the left operand alone decides the result.
-- loop(_) diverges, so this test times out if the desugaring is
-- not lazy in the right operand.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun loop :: Bool -> Bool =
    b -> loop(b)

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put if False && loop(True) then "wrong" else "short and" on _console
        hput ConsolePut on _console
        put if True || loop(True) then "short or" else "wrong" on _console
        hput ConsoleClose on _console
        halt _console
