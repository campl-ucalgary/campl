-- A file for testing valid cases of infix operators.
-- TODO: also need to test infix operators inside unfolds, records.

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

data List () -> Z =
    Cons :: Int,Z -> Z
    Nil :: -> Z

-- typed declaration and recursive application
fun (>>) :: Int,Int -> Int =
    a,b -> (>>)(a >> a,(>>)(a,b) >> (>>)(b,a))

-- untyped declaration and repeated application 
fun (<<) =
    a,b -> a << b << a << b << a

-- inside a 'defn/while' block.
defn
    fun (>_<) =
        a,b -> 0
where
    fun (-_-) =
        a,b -> 0

-- inside every possible expression construct
fun (^-^) =
    a,b -> 0

fun head = -- to help with testing.
    b:bs -> b
    [] -> 0

fun first = -- to help with testing.
    (a,_,_) -> a

fun everyExpr =
    0 -> -- 'if' statement
        if (0 ^-^ 0) == 0
            then (0 ^-^ 0)
            else (0 ^-^ 1)
    1 -> -- 'let' statement
        let
            fun q =
                a -> 0 ^-^ 0
        in q(0) ^-^ q(0)
    -- note: here we need to check if it works on the left and right.
    -- For whatever reason, &&,||, and ++ haven't actually been implemented, so we can't test if this works.
    -- It should work, though.
    {-
    2 -> -- boolean operators
        if (True && (True || ((0 ^-^ 0) == 0) || True) && True)
            then 0
            else 1
    3 -> -- list operators
        head([0] ++ [0 ^-^ 0] ++ [0])
    -}
    4 -> -- integer operators (except '^', which also hasn't been implemented)
        (1 * (1 + (0 ^-^ 0) + 1) * 1)
    {-
    5 -> -- Not sure what this operator does, but it hasn't been implemented.
        if 0 !! (0 ^-^ 0)
            then 0
            else 1
    -}
    6 -> -- list operators, excluding '++'
        head([0 ^-^ 0])
    7 -> -- folds
        fold Cons(0^-^0,Nil) of
            Nil: -> 1^-^1
            Cons:n,ns -> 8^-^8
    8 -> case (0^-^0) of
        0 -> (0^-^0)
        _ -> (0^-^0)
    9 -> switch
        (0^-^0) < 0 -> (0^-^0)
        True -> (0^-^0)
    10 -> first((0^-^0,0^-^0,0^-^0)) -- testing tuples
    11 -> ((((((first((0^-^0,0^-^0,0^-^0)))))))) -- bracketed expressions.
    a -> 0^-^0^-^0^-^0^-^0^-^0^-^0^-^0^-^0^-^0 -- Nothing special, we've already tested this.

-- Testing valid names:
fun (|@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (&@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (<@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (>@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (!@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (+@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (-@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (*@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (/@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (%@#$_?|:~=|&<>!+-*/%^) =
    a -> a

fun (^@#$_?|:~=|&<>!+-*/%^) =
    a -> a

-- Some simple functions to help demonstrate order-of-operations.

fun (+#) = -- append, since '++' is already a thing.
    [],c -> c
    b:bs,c -> b:(bs +# c)

fun (|_) =
    a,b -> ... -- TODO make this work.


-- A couple tests on order-of-operations
proc helloworld :: | Console => = 
    | console => -> do
        
        hput ConsolePut on console
        put "Enter your name" on console
        
        on console do
            hput ConsoleGet
            get name
            
            hput ConsolePut
            put "Hello,"
            hput ConsolePut
            put name
        
        hput ConsolePut on console 
        put "Hello World" on console
        
        hput ConsoleClose on console
        halt console

proc run = 
    | console => -> helloworld( |console=>)