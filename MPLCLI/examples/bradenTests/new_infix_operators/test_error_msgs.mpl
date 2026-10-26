


data List () -> Z =
    Cons :: Int,Z -> Z
    Nil :: -> Z 

fun bool_to_string =
    True -> "True"
    False -> "False"

proc run =
    | console => -> on console do
            hput ConsolePut
            put bool_to_string(True < False)
            hput ConsoleClose
            halt
