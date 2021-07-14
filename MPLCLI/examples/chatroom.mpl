{-
protocol StringTerminal => S =
    StringTerminalPut :: Put( [Char] | S) => S
    StringTerminalGet :: Get( [Char] | S) => S 
    StringTerminalClose :: TopBot => S

coprotocol Console => S =
    ConsolePut   :: S => Get( [Char] | S) 
    ConsoleGet   :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

protocol 
    Passer( | M) => S =
        Passer :: M (+) ( Neg(M) (*) S) => S

-- | gets the inputs
defn 
    proc getInps :: | Get([Char] | TopBot) (+) Get([Char] | TopBot) => StringTerminal, StringTerminal =
        | ch => _strterm0, _strterm1 -> do
            fork ch as 
                ch0 with _strterm0 -> go( | ch0 => _strterm0)
                ch1 with _strterm1 -> go( | ch1 => _strterm1)
where
    proc go :: | Get([Char] | TopBot) => StringTerminal =
        | ch => _strterm -> do
            hput StringTerminalGet on _strterm
            get inp on _strterm

            hput StringTerminalClose on _strterm
            close _strterm

            put inp on ch

            halt ch

-- | gets the inputs
defn
    proc parallelGet :: | => Get([Char] | TopBot), Get([Char] | TopBot), StringTerminal =
        | => ch0, ch1, _strterm -> do
            race 
                ch0 -> go( | => ch0, ch1, _strterm)
                ch1 -> go( | => ch1, ch0, _strterm)
where
    proc go :: | => Get([Char] | TopBot), Get([Char] | TopBot), StringTerminal =
        | => chwin, chlose, _strterm -> do
            get n on chwin 

            hput StringTerminalPut on _strterm
            put n on _strterm

            get _ on chlose

            close chwin
            close chlose

            hput StringTerminalClose on _strterm
            halt _strterm

proc run :: | => StringTerminal, StringTerminal, StringTerminal = 
    | => _strterm0, _strterm1, _strterm2 -> do
        plug 
            getInps( | ch => _strterm0, _strterm1)
            => ch, _strterm2 -> do
                split ch into ch0, ch1
                parallelGet( | => ch0, ch1, _strterm2)

-}
