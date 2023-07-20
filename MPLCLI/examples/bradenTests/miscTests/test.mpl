

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun run :: [Int] -> Int =
    _ -> 0

fun run :: Int -> Int =
    a -> a

proc run =
    | => a -> do
        halt a

proc p1 =
    | => a -> do
        get _ on a
        halt a

proc p2 =
    | a => -> do
        put run(0) on a
        halt a

proc run =
    |  => -> do
        plug
            p1(|=>a)
            p2(|a=>)