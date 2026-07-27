{-
True
False
True
False
True
-}

-- Built-in short-circuit boolean operators: || and && desugar to
-- if-expressions (MplPasses.Parser.ResolveBoolOps) when the program
-- does not define the operator itself. && (precedence 2) binds
-- tighter than || (precedence 1): False && True || True is
-- (False && True) || True. The sectioned forms (||)(_, _) and
-- (&&)(_, _) go through the same path.

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot

fun showBool :: Bool -> [Char] =
    b -> if b then "True" else "False"

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put showBool(True || False) on _console
        hput ConsolePut on _console
        put showBool(True && False) on _console
        hput ConsolePut on _console
        put showBool(False && True || True) on _console
        hput ConsolePut on _console
        put showBool((&&)(True, False)) on _console
        hput ConsolePut on _console
        put showBool((||)(False, True)) on _console
        hput ConsoleClose on _console
        halt _console
