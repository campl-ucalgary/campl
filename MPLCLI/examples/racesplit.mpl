protocol StringTerminal => S =
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalClose :: TopBot => S


{-
protocol Chain (| M ) => S = 
    Link :: M (+) S =>  S
    End :: M => S
-}

coprotocol S => CoChain (| M )  = 
    CoLink ::  S => M (+) S
    CoEnd :: S => TopBot 


proc raceChain :: | => Chain( | Get([Char] | TopBot) ) =
    | => chain -> hcase chain of    
        CoLink -> do
            split chain into gettb, nchain
            -- race gettb
            plug 
                
        CoEnd -> halt chain
        




