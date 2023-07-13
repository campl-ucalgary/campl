-- This is a full test of every syntactic device and use of the 'on' directive.
-- If this compiles, and the AST looks right, then the 'on' syntax is working.

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 

protocol Nope => S =
    NothingToDoHere :: TopBot => S
    SomethingToDoHere :: Put(Int | S) => S

-- 1-line functions
proc test1 =
    | => channel -> on channel do
        put 3
        get _
        halt

-- Recursing into sub-blocks of 'if'
proc test2 =
    | => channel -> do
        if 4 < 5
            then
                if 4 < 6
                    then
                        on channel do
                            put 2
                            halt
                    else
                        on channel do
                            put 3
                            halt
            else do
                on channel do
                    put 4
                    halt

-- Recursing into sub-blocks of 'case'
proc test3 =
    | => channel -> do
        case [] of
            [] -> case [] of
                    [] -> on channel do
                            put 2
                            halt
                    _ -> on channel do
                            put 3
                            halt
            _ -> do
                on channel do
                    put 4
                    halt

-- Recursing into sub-blocks of 'switch'
proc test4 =
    | => channel -> do
        switch
            3 < 4 -> switch
                3 < 4 -> on channel do
                    put 1
                    halt
                True -> do
                    on channel do
                        put 2
                        halt
            3 < 5 -> on channel do
                put 3
                halt
            True -> do
                on channel do
                    put 4
                    halt

-- Test the simple primitives
proc test5 =
    | c2 => channel -> do
        on channel do
            put 0
            get _
            hput NothingToDoHere
            close
        halt c2

-- race and plug
proc test6 =
    | => channel -> do
        split channel into a,b
        race
            a -> do
                plug
                    c=>a -> do
                        get _ on a
                        close a
                        on c do
                            get _
                            halt
                    =>c -> on c do
                        put 0
                        halt
            b -> do
                on b do
                    get _
                    close
                on a do
                    get _
                    halt

-- split
proc test7 =
    | => channel -> do
        on channel do
            get _
            split into a,b
        on a do
            put 0
            close
        on b do
            halt

-- fork
proc test8 =
    | => channel -> do
        on channel do
            fork as
                a -> do
                    on a do
                        fork as
                            a1 -> on a1 do
                                halt
                            a2 -> on a2 do
                                halt
                b -> on b do
                    halt

-- hcase
proc test9 =
    | channel => -> do
        on channel do
            hcase of
                NothingToDoHere -> on channel do
                    halt
                SomethingToDoHere -> do
                    on channel do
                        get _
                        hcase of
                            NothingToDoHere -> on channel do
                                halt
                            SomethingToDoHere -> do
                                get _ on channel
                                test9(|channel=>)


proc run = 
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