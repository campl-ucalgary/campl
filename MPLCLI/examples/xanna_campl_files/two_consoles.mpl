
include Prelude

proc one_console =
    int | console => dummy -> do
        close dummy
        on console do
            hput IntConsolePut
            put int
            hput ConsoleClose
            halt

proc dummy = 
    | dummy1, dummy2 => -> do
        close dummy1
        halt dummy2


proc run =
    | console1, console2 => -> plug
        dummy( | dummy1, dummy2 => )
        one_console( 1 | console1 => dummy1 )
        one_console( 2 | console2 => dummy2 )