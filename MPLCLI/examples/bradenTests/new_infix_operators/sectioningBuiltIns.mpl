-- This is to show off the sectioning of built-in operators.


coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

-- Used to make sure it is properly recursing into the subtrees.
fun (+++) =
    a,b -> 0

-- Note: since some of these operators haven't been implemented,
-- we comment out any line that would throw an error for that reason.

fun a =
    0 -> (+)(0+++0,0+++0)
    1 -> (-)(0+++0,0+++0)
    2 -> (*)(0+++0,0+++0)
    -- 3 -> (/)(0+++0,0+++0)
    -- 4 -> (%)(0+++0,0+++0)
    -- 5 -> (^)(0+++0,0+++0)
    -- 6 -> (||)(0+++0,0+++0)
    -- 7 -> (&&)(0+++0,0+++0)
    -- 8 -> (!!)(0+++0,0+++0)
    n -> n

fun b =
    0 -> (==)(0+++0,0+++0)
    1 -> (>=)(0+++0,0+++0)
    2 -> (<=)(0+++0,0+++0)
    -- 3 -> (/=)(0+++0,0+++0)
    4 -> (<)(0+++0,0+++0)
    -- 5 -> (>)(0+++0,0+++0)
    n -> False

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> on console do
        hput ConsolePut
        put "enter anything to finish"
        hput ConsoleGet
        get _
        hput ConsoleClose
        halt

proc run = 
    | console => -> helloworld( |console=>)