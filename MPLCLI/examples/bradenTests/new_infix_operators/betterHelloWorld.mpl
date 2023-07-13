-- A file for testing valid cases of infix operators.

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

data List () -> Z =
    Cons :: Int,Z -> Z
    Nil :: -> Z

-- An endless list
codata Z -> EList() =
    Head :: Z -> Int
    Tail :: Z -> Z

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


fun (++) =
    a,b -> a + b


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
    -- 7 -> -- folds (NOTE: not actually implemented)
        -- fold Cons(0^-^0,Nil) of
            -- Nil: -> 1^-^1
            -- Cons:n,ns -> 8^-^8
    8 -> case (0^-^0) of
        0 -> (0^-^0)
        _ -> (0^-^0)
    9 -> switch
        (0^-^0) < 0 -> (0^-^0)
        True -> (0^-^0)
    10 -> first((0^-^0,0^-^0,0^-^0)) -- testing tuples
    11 -> ((((((first((0^-^0,0^-^0,0^-^0)))))))) -- bracketed expressions.
    a -> 0^-^0^-^0^-^0^-^0^-^0^-^0^-^0^-^0^-^0 -- Nothing special; we've already tested this.

-- Testing valid names:
fun (|@#$_?|:~|&<>!+-*/%^) =
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

-- records
fun abc :: Int -> EList =
    n -> (Head := -> (n^-^n) , Tail := -> abc(n + 1))

-- unfolds (NOTE: not actually implemented)
-- fun abd =
    -- n -> unfold (n^-^n) of
        -- r of
            -- Head : -> (r^-^r)
            -- Tail : -> (r^-^r)

-- dummy process, for testing
proc test2 =
    a | => c1 -> do
        put 0 on c1
        halt c1

-- process commands
proc test =
    | => c1, c2 -> do
        on c1 do
            put 0^-^0
            close
        if (0^-^0) < 0
            then do
                case 0^-^0 of
                    0 -> do
                        put 0^-^0 on c2
                        halt c2
                    n -> test2(0^-^0 | => c2)
            else do
                switch
                    (0^-^0) < 0 -> on c2 do
                        put (0^-^0)
                        halt
                    True -> on c2 do
                        put 0
                        halt



-- A simple-enough hello-world program. Just a stand-in, really.
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