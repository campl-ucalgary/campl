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
    5 -> (>)(0+++0,0+++0)
    n -> False

-- testing primitive bool operations
fun c =
    0 -> (==)(0++++0,0++++0)
    1 -> (||)(0++++0,0++++0)
    2 -> (&&)(0++++0,0++++0)
    -- 2 -> (!!)(0++++0,0++++0)
    n -> False

-- testing primitive char operations
fun d =
    0 -> (==)(0***0,0***0)
    1 -> (>=)(0***0,0***0)
    2 -> (<=)(0***0,0***0)
    -- 3 -> (/=)(0***0,0***0)
    4 -> (<)(0***0,0***0)
    5 -> (>)(0***0,0***0)
    n -> False

-- now let's do a weird test that should not work:

-- custom data types should not work with primitive == eq comparison
data Other () -> Z =
    Val :: Int -> Z
    
fun (+??) =
    a,b -> Val(0)

-- fun e =
--     0 -> (==)(0+??0,0+??0)       -- this should cause an error, ideally at type check
--     n -> False

-- -- it did not originally do this however.
-- -- previously it gave the error was:
-- -- assembler error "mpl: illegal use of eq instruction on unsupported type (TODO: make this error message better). 
-- -- CallStack (from HasCallStack):
-- --   error, called at src/MplAsmPasses/FromLambdaLifted/FromLambdaLifted.hs:397:26 in MPLASM-0.1.0.0-7ksTEgDj5MY73yVbDTyaRv:MplAsmPasses.FromLambdaLifted.FromLambdaLifted"

-- -- then i messed around with the type checker
-- -- now the error we get is:
-- -- mpl: type check / semantic error:
-- --  •  Match failure with types

-- --         Equatable (T781)

-- --     and

-- --         Other ()

-- --     arising from expression

-- --         +?? (0, 0)

-- --     at line 69 and column 16 and expression

-- --         +?? (0, 0)

-- --     at line 69 and column 16


fun show_bool :: Bool -> [Char] =
    True -> "True"
    False -> "False"

-- A simple function.
proc helloworld :: | Console => = 
    | console => -> do
        let a = 'a'
            b = 'b'
        in if (a <= b)
            then on console do
                hput ConsolePut
                put "testing char leq 'a' <= 'b': " ++ show_bool(a <= b)
                hput ConsoleClose
                halt
            else on console do
                hput ConsoleClose
                halt

proc run = 
    | console => -> helloworld( |console=>)