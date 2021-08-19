{- | This program takes 3 channels, and ``tangles'' them up,
so that we can unravel them and print stuff to their consoles.
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

protocol Wire( | M) => S =
    Wire :: M (*) (TopBot (+) S) => S 
    Strand :: M => S


coprotocol S => CoWire( | M) =
    CoWire :: S => M (*) (TopBot (+) S)
    CoStrand :: S => M 

proc tangle1 :: | CoWire( | M) => M = 
    | cowire => m0 -> do
        hput CoStrand on cowire
        cowire |=| m0

proc tangle2 :: | CoWire( | M) => M, M = 
    | cowire => m0,m1 -> do
        hput CoWire on cowire
        split cowire into nm0,strand0
        fork strand0 as
            fray -> do
                close fray 
                nm0 |=| m0
            cowire -> tangle1( | cowire => m1)

proc tangle3 :: | CoWire( | M) => M,M,M = 
    | cowire => m0,m1,m2 -> do
        hput CoWire on cowire
        split cowire into nm0, strand0

        fork strand0 as
            fray -> do  
                close fray
                nm0 |=| m0
            cowire -> tangle2( | cowire => m1,m2)


proc testPutter :: | => CoWire(| StringTerminal) =
    | => _cowire -> hcase _cowire of  
        CoWire -> do
            fork _cowire as 
                _strterm -> do
                    hput StringTerminalPut on _strterm
                    put "dog" on _strterm

                    hput StringTerminalClose on _strterm
                    halt _strterm
                strand -> do
                    split strand into fray,cowire
                    close fray 
                    testPutter( | => cowire)
        CoStrand -> do
            hput StringTerminalPut on _cowire
            put "end" on _cowire

            hput StringTerminalClose on _cowire
            halt _cowire
                    
proc run =
    | => _strterm0, _strterm1, _strterm2 -> do
        plug
            tangle3(| cowire => _strterm0, _strterm1, _strterm2)
            testPutter( | => cowire)

