{- | This program takes 3 channels, and ``tangles'' them up,
so that we can unravel them and print stuff to their consoles.
-}

protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S

coprotocol S => Console =
    ConsolePut   :: S => Get( [Char] | S) 
    ConsoleGet   :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

{-
protocol Wire( | M) => S =
    -- Wire :: M (*) (TopBot (+) S) => S 
    Strand :: Neg(M) (*) M => S

coprotocol S => CoWire( | M) =
    -- CoWire :: S => M (*) (TopBot (+) S)
    CoStrand :: S => M 
    -- CoWire :: S => M (*) (TopBot (+) S)
-}

protocol ProxyStringTerminal => S =
    ProxyPut :: Put([Char] | S) => S 
    ProxyGet :: Get([Char] | S) => S
    ProxyClose :: TopBot => S


coprotocol S => CoWire( | M) =
    CoWire :: S => M (*) (TopBot (+) S)
    CoStrand :: S => M 

protocol RePass( | M) => S =
    RePass :: M (*) (Neg(Neg(M)) (+) S) => S 


        
{-
proc run =
    -- | => _strterm0, _strterm1, _strterm2 -> do
    -- | _console => _strterm0 -> do
    | => strterm0, strterm1 -> do
        plug
            testWired( | wire => strterm1)
            retangle1( | cowire => wire, proxy)
            proxyTerm( | proxy => strterm0)
            testCoWired( | => cowire )
-}
