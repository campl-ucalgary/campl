

coprotocol S => Console = 
    ConsolePut :: S => Get( [Char] | S)
    ConsoleGet :: S => Put( [Char] | S)
    ConsoleClose :: S => TopBot 


protocol Closer => S =
    Closer :: TopBot => S

-- This should not compile, because 'a' is immediately out-of-scope.
-- The error we get should give the correct line numbers for each case (errors on lines 20-22,25-30)
proc oof =
    | => a -> do
        on a do
            fork as
                b -> halt b
                c -> halt c
            get a
            put a
            fork as
                b -> halt b
                c -> halt c
            split into b2,c2
            hcase of
                Closer -> halt a
            hput Closer
            close
            halt









proc run = 
    | console => -> do
        
        hput ConsoleClose on console
        halt console