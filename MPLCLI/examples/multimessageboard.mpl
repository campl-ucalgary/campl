{- yet another failed attempt... -}
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot

    -- we would have to fork for that... thats the idea
    ConsoleStringTerminal :: S => S (+) Neg(StringTerminal)

{-
coprotocol S => CoWire( | M) =
    CoWire :: S => M (*) (TopBot (+) S)
    CoStrand :: S => M 
-}


coprotocol S => ListP( | M) =
    ListP :: S => M (+) S
    NilP :: S => TopBot

-- | a raceable board writer (should wrap this in @Put( () | )@ for it to be raceable)
protocol BoardWriter => S = 
    GetWritten :: Get ( [Char] | Get( () | S)) => S

defn 
    proc node :: | Get( () | BoardWriter) => Get( () | BoardWriter), ListP( | Get( () | BoardWriter) )  = 
        | up => now, listp -> hcase listp of
            ListP -> do
                split listp into below, nlistp
                race 
                    now -> goRace( | up => now, below, nlistp) 
                    below -> goRace( | up => below, now, nlistp) 
            NilP -> goNil( | up => now, listp)

    -- | goes and does the race 
    proc goRace :: | Get( () | BoardWriter) => Get( () | BoardWriter), Get( () | BoardWriter),  ListP( | Get( () | BoardWriter)) = 
        | up => winner, loser, nlistp -> do
            get () on winner 

            hput GetWritten on winner 
            get inp on winner

            plug 
                up => winner, nnlistp -> do
                    put () on up
                    hcase up of
                        GetWritten -> do
                            put inp on up
                            node( | up => winner, nnlistp )
                nnlistp => loser, nlistp -> do
                    get () on loser 
                    hput GetWritten on loser 
                    get inp on loser

                    hput ListP on nnlistp 
                    fork nnlistp as 
                        nloser -> do
                            put () on nloser 
                            hcase nloser of
                                GetWritten -> do
                                    put inp on nloser
                                    nloser |=| loser
                        nwinner -> nwinner |=| nlistp

    proc goNil :: | Get( () | BoardWriter) => Get( () | BoardWriter), TopBot = 
        | up => now, tb -> do
            get () on now
            put () on up

            hput GetWritten on now
            get inp on now 

            hcase up of
                GetWritten -> do
                    put inp on up 
                    goNil( | up =>  now, tb)
