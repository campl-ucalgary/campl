-- A demonstration of order-of-operations

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

fun (+++) = -- append, since '++' is already a thing.
    [],c -> c
    b:bs,c -> b:(bs +++ c)

fun (|_) =
    a,b -> "(" +++ a +++ " |_ " +++ b +++ ")"

fun (&_) =
    a,b -> "(" +++ a +++ " &_ " +++ b +++ ")"

fun (>_) =
    a,b -> "(" +++ a +++ " >_ " +++ b +++ ")"

fun (+_) =
    a,b -> "(" +++ a +++ " +_ " +++ b +++ ")"

fun (*_) =
    a,b -> "(" +++ a +++ " *_ " +++ b +++ ")"

fun (^_) =
    a,b -> "(" +++ a +++ " ^_ " +++ b +++ ")"


-- A couple tests on order-of-operations
proc helloworld :: | Console => = 
    | console => -> on console do
            
            -- All the associativity.
            -- Everything is left-associative, except '^', which is right-associative
            hput ConsolePut
            put "Everything is left-associative, except '^', which is right-associative"
            
            hput ConsolePut
            put "1" |_ "2" |_ "3"
            
            hput ConsolePut
            put "1" &_ "2" &_ "3"
            
            hput ConsolePut
            put "1" >_ "2" >_ "3"
            
            hput ConsolePut
            put "1" +_ "2" +_ "3"
            
            hput ConsolePut
            put "1" *_ "2" *_ "3"
            
            hput ConsolePut
            put "1" ^_ "2" ^_ "3"
            
            hput ConsolePut
            put "\nOrder of operations: smaller numbers should be in more layers of brackets"
            
            hput ConsolePut
            put "6" |_ "5" &_ "4" >_ "3" +_ "2" *_ "1" ^_ "1"
            
            hput ConsolePut
            put "1" ^_ "1" *_ "2" +_ "3" >_ "4" &_ "5" |_ "6"
        
            hput ConsolePut
            put "\nDone"
        
            hput ConsoleClose
            halt

proc run = 
    | console => -> helloworld( |console=>)