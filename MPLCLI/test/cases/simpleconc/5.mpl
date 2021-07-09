{-
a
b
aa
bb
aaa
bbb
aaaa
bbbb
aaaaa
bbbbb
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

protocol Putter(A | ) => S  =
    Putter :: Put(A | S)  => S
    Stop   :: TopBot  => S


proc p1 :: Int, ([Char], [Char]) | => Putter( ([Char], [Char]) | ) =
    0, _ | => ch -> do
        hput Stop on ch
        halt ch
    n, (l, r) | => ch -> do
        hput Putter on ch
        case ('a':l,'b':r ) of
            res -> do
                put res on ch
                p1( n - 1, res | => ch )


proc p2 :: | Console, Putter( ([Char], [Char]) | )  => =
    | _console, ch  => -> hcase ch of 
        Putter -> do
            get (l,r) on ch

            hput ConsolePut on _console
            put l on _console
            hput ConsolePut on _console
            put r on _console

            p2( | _console, ch => )

        Stop -> do
            hput ConsoleClose on _console
            close _console
            halt ch

proc run =
    | _console => ->  do
        plug
            p1( 5, ("","")|              => ch )
            p2( | _console, ch =>    )
