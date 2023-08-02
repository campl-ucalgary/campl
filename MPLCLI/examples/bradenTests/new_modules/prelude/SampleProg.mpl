
include Prelude

proc runH =
    | => term -> do
        on term do
            hput StringTerminalPut
            put "Input something:"
            hput StringTerminalGet
            get a
            hput StringTerminalPut
            put "You inputted: '" ++ a ++ "'\n(enter to close)"
            hput StringTerminalGet
            get a
            hput StringTerminalClose
            halt

proc run :: | Console => =
    | con => -> do
        on con do
            hput ConsolePut
            put "Input something:"
            hput ConsoleGet
            get a
            hput ConsolePut
            put "You inputted: '" ++ a ++ "'"
            hput ConsoleStringTerminal
            split into con2,term
        on con2 do
            hput ConsolePut
            put "done" ++ "."
            hput ConsoleClose
            close
        plug
            runH(|=>termNeg)
            termNeg,term => -> term |=| neg termNeg
        