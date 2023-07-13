-- A demonstration of order-of-operations

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- Some functions to show off order-of-operations in a printable way.

fun (++) = -- append
    [],c -> c
    b:bs,c -> b:(bs ++ c)

fun (||) =
    a,b -> "(" ++ a ++ " || " ++ b ++ ")"

fun (&&) =
    a,b -> "(" ++ a ++ " && " ++ b ++ ")"

fun (>>) =
    a,b -> "(" ++ a ++ " >> " ++ b ++ ")"

fun (+_+) =
    a,b -> "(" ++ a ++ " +_+ " ++ b ++ ")"

fun (**) =
    a,b -> "(" ++ a ++ " ** " ++ b ++ ")"

fun (^) =
    a,b -> "(" ++ a ++ " ^ " ++ b ++ ")"


-- A couple tests on order-of-operations
proc helloworld :: | Console => = 
    | console => -> on console do
            
            -- All the associativity.
            -- Everything is left-associative, except '^', which is right-associative
            hput ConsolePut
            put "Everything is left-associative, except '^', which is right-associative"
            
            hput ConsolePut
            put "1" || "2" || "3"
            
            hput ConsolePut
            put "1" && "2" && "3"
            
            hput ConsolePut
            put "1" >> "2" >> "3"
            
            hput ConsolePut
            put "1" +_+ "2" +_+ "3"
            
            hput ConsolePut
            put "1" ** "2" ** "3"
            
            hput ConsolePut
            put "1" ^ "2" ^ "3"
            
            hput ConsolePut
            put "\nOrder of operations: smaller numbers should be more tightly-bound."
            
            hput ConsolePut
            put "6" || "5" && "4" >> "3" +_+ "2" ** "1" ^ "1"
            
            hput ConsolePut
            put "1" ^ "1" ** "2" +_+ "3" >> "4" && "5" || "6"
        
            hput ConsolePut
            put "\nDone"
        
            hput ConsoleClose
            halt

proc run = 
    | console => -> helloworld( |console=>)