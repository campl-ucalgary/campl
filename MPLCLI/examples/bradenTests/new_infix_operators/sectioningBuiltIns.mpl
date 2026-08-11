-- This is to show off the sectioning of built-in operators.


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- Used to make sure it is properly recursing into the subtrees.
fun (+++) =
    a,b -> 0

fun (++++) =
    a,b -> True

fun (***) =
    a,b -> 'a'

-- Note: since some of these operators haven't been implemented,
-- we comment out any line that would throw an error for that reason.

fun a =
    0 -> (+)(0+++0,0+++0)
    1 -> (-)(0+++0,0+++0)
    2 -> (*)(0+++0,0+++0)
    3 -> (/)(0+++0,0+++0)
    4 -> (%)(0+++0,0+++0)
    -- 5 -> (^)(0+++0,0+++0)
    n -> n

fun b =
    0 -> (==)(0+++0,0+++0)
    1 -> (>=)(0+++0,0+++0)
    2 -> (<=)(0+++0,0+++0)
    -- 3 -> (/=)(0+++0,0+++0)
    4 -> (<)(0+++0,0+++0)
    -- 5 -> (>)(0+++0,0+++0)
    n -> False

fun c =
    0 -> (||)(0++++0,0++++0)
    1 -> (&&)(0++++0,0++++0)
    -- 2 -> (!!)(0++++0,0++++0)
    n -> False

-- testing that primitive char comparison works
fun d =
    0 -> (==)('a' *** 'a','a' *** 'a')
    n -> False

-- can be used to implement a string comp function 
fun (&==) :: [Char], [Char] -> Bool =
    [], [] -> True
    (x:xs), [] -> False
    [], (x:xs) -> False
    (x:xs), (y:ys) -> (x == y) && (xs &== ys)

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> do
        if ("hi" &== "hi")
            then on console do
                hput ConsolePut
                put "enter anything to finish"
                hput ConsoleGet
                get _
                hput ConsoleClose
                halt
            else on console do
                hput ConsoleClose
                halt

proc run = 
    | console => -> helloworld( |console=>)